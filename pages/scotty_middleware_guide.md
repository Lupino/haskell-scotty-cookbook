# Haskell Scotty 中间件系统完整指南

## 1. 中间件概念和作用

### 什么是中间件

中间件是位于 HTTP 请求处理链中的函数，它们可以在请求到达路由处理器之前或响应返回给客户端之前执行特定的逻辑。在 Scotty 中，中间件基于 WAI (Web Application Interface) 构建。

### 中间件的作用

- **预处理请求**：验证、解析、记录日志
- **后处理响应**：添加头部、压缩、缓存
- **横切关注点**：身份验证、CORS、错误处理
- **请求链管理**：控制请求流向和中断

### 基本结构

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Data.Time

-- 基本的中间件类型
type Middleware = Application -> Application
```

## 2. 自定义中间件编写

### 简单的时间记录中间件

```haskell
import Data.Time
import qualified Data.ByteString.Char8 as BS

-- 计时中间件
timingMiddleware :: Middleware
timingMiddleware app req respond = do
    start <- getCurrentTime
    app req $ \res -> do
        end <- getCurrentTime
        let duration = diffUTCTime end start
        putStrLn $ "Request took: " ++ show duration
        respond res

-- 在 Scotty 应用中使用
main :: IO ()
main = scotty 3000 $ do
    middleware timingMiddleware
    
    get "/" $ do
        text "Hello World!"
```

### 请求信息记录中间件

```haskell
import Network.Wai
import Network.HTTP.Types
import qualified Data.ByteString.Char8 as BS

-- 请求信息中间件
requestInfoMiddleware :: Middleware
requestInfoMiddleware app req respond = do
    let method = requestMethod req
        path = rawPathInfo req
        query = rawQueryString req
    
    putStrLn $ "Method: " ++ BS.unpack method
    putStrLn $ "Path: " ++ BS.unpack path
    putStrLn $ "Query: " ++ BS.unpack query
    
    app req respond

-- 使用示例
scottyApp :: IO ()
scottyApp = scotty 3000 $ do
    middleware requestInfoMiddleware
    
    get "/users/:id" $ do
        userId <- param "id"
        json $ object ["user_id" .= (userId :: String)]
```

### 响应头添加中间件

```haskell
import Network.HTTP.Types
import Network.Wai

-- 添加安全头部中间件
securityHeadersMiddleware :: Middleware
securityHeadersMiddleware app req respond = 
    app req $ respond . addHeaders
  where
    addHeaders res = 
        let headers = [
                ("X-Frame-Options", "DENY"),
                ("X-Content-Type-Options", "nosniff"),
                ("X-XSS-Protection", "1; mode=block"),
                ("Strict-Transport-Security", "max-age=31536000")
            ]
        in mapResponseHeaders (++ headers) res
```

## 3. 日志记录中间件

### 使用内置日志中间件

```haskell
import Network.Wai.Middleware.RequestLogger
import System.Log.FastLogger

-- 使用标准日志格式
standardLoggingApp :: IO ()
standardLoggingApp = scotty 3000 $ do
    middleware logStdoutDev  -- 开发环境日志
    -- middleware logStdout   -- 生产环境日志
    
    get "/" $ text "Hello with logging!"
```

### 自定义日志中间件

```haskell
import Data.Time
import System.IO
import Control.Exception
import qualified Data.ByteString.Char8 as BS

-- 自定义日志记录器
data LogLevel = INFO | WARN | ERROR deriving (Show)

logMessage :: LogLevel -> String -> IO ()
logMessage level msg = do
    time <- getCurrentTime
    let logEntry = show time ++ " [" ++ show level ++ "] " ++ msg
    putStrLn logEntry
    hFlush stdout

-- 详细日志中间件
detailedLoggingMiddleware :: Middleware
detailedLoggingMiddleware app req respond = do
    let method = BS.unpack $ requestMethod req
        path = BS.unpack $ rawPathInfo req
        userAgent = maybe "Unknown" BS.unpack $ 
                   lookup "User-Agent" (requestHeaders req)
    
    logMessage INFO $ "Request: " ++ method ++ " " ++ path
    logMessage INFO $ "User-Agent: " ++ userAgent
    
    start <- getCurrentTime
    
    app req $ \res -> do
        end <- getCurrentTime
        let duration = diffUTCTime end start
            status = statusCode $ responseStatus res
        
        logMessage INFO $ "Response: " ++ show status ++ 
                         " (took " ++ show duration ++ ")"
        respond res

-- 错误捕获日志中间件
errorLoggingMiddleware :: Middleware
errorLoggingMiddleware app req respond = 
    catch (app req respond) $ \e -> do
        logMessage ERROR $ "Exception: " ++ show (e :: SomeException)
        respond $ responseLBS status500 [] "Internal Server Error"
```

### 文件日志中间件

```haskell
import System.IO
import Control.Concurrent.MVar
import Data.Time.Format

-- 文件日志配置
data FileLogger = FileLogger {
    logHandle :: Handle,
    logMutex :: MVar ()
}

createFileLogger :: FilePath -> IO FileLogger
createFileLogger path = do
    handle <- openFile path AppendMode
    hSetBuffering handle LineBuffering
    mutex <- newMVar ()
    return $ FileLogger handle mutex

writeLog :: FileLogger -> String -> IO ()
writeLog logger msg = withMVar (logMutex logger) $ \_ -> do
    time <- getCurrentTime
    let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" time
    hPutStrLn (logHandle logger) $ timeStr ++ " " ++ msg

-- 文件日志中间件
fileLoggingMiddleware :: FileLogger -> Middleware
fileLoggingMiddleware logger app req respond = do
    let method = BS.unpack $ requestMethod req
        path = BS.unpack $ rawPathInfo req
    
    writeLog logger $ "REQUEST " ++ method ++ " " ++ path
    
    app req $ \res -> do
        let status = statusCode $ responseStatus res
        writeLog logger $ "RESPONSE " ++ show status
        respond res
```

## 4. 身份认证中间件

### 基本认证中间件

```haskell
import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types

-- 简单的基本认证
basicAuthMiddleware :: String -> String -> Middleware
basicAuthMiddleware username password app req respond = do
    case lookup "Authorization" (requestHeaders req) of
        Nothing -> unauthorized
        Just authHeader -> 
            if validAuth authHeader
                then app req respond
                else unauthorized
  where
    unauthorized = respond $ responseLBS status401 
        [("WWW-Authenticate", "Basic realm=\"Restricted\"")] 
        "Unauthorized"
    
    validAuth header = 
        case BS.stripPrefix "Basic " header of
            Nothing -> False
            Just encoded -> 
                case decode encoded of
                    Left _ -> False
                    Right decoded -> 
                        decoded == BS.pack (username ++ ":" ++ password)

-- 使用示例
protectedApp :: IO ()
protectedApp = scotty 3000 $ do
    middleware $ basicAuthMiddleware "admin" "secret"
    
    get "/admin" $ do
        text "Welcome to admin panel!"
```

### JWT 认证中间件

```haskell
import Web.JWT
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

-- JWT 配置
jwtSecret :: Secret
jwtSecret = secret "your-secret-key"

-- JWT 认证中间件
jwtAuthMiddleware :: [String] -> Middleware  -- 排除的路径
jwtAuthMiddleware excludePaths app req respond = do
    let path = BS.unpack $ rawPathInfo req
    
    if path `elem` excludePaths
        then app req respond
        else case extractToken req of
            Nothing -> unauthorized "Missing token"
            Just token -> 
                case verifyToken token of
                    Nothing -> unauthorized "Invalid token"
                    Just claims -> do
                        -- 将用户信息添加到请求中
                        let newReq = addUserToRequest claims req
                        app newReq respond
  where
    unauthorized msg = respond $ responseLBS status401 [] msg
    
    extractToken req = do
        authHeader <- lookup "Authorization" (requestHeaders req)
        BS.stripPrefix "Bearer " authHeader
    
    verifyToken tokenBS = do
        let tokenText = T.pack $ BS.unpack tokenBS
        jwt <- decode tokenText
        if verify jwtSecret jwt
            then Just $ claims jwt
            else Nothing
    
    addUserToRequest claims req = 
        -- 这里可以将用户信息添加到请求的 vault 中
        req

-- JWT 工具函数
createJWT :: String -> IO (Maybe Text)
createJWT userId = do
    now <- getCurrentTime
    let claims = def {
        iss = stringOrURI "your-app",
        Web.JWT.exp = numericDate $ addUTCTime 3600 now,  -- 1小时过期
        unregisteredClaims = Map.fromList [("user_id", String $ T.pack userId)]
    }
    return $ Just $ encodeSigned HS256 jwtSecret claims
```

### 会话认证中间件

```haskell
import qualified Data.Map as Map
import Data.IORef
import System.Random
import Control.Concurrent.STM

-- 会话存储
type SessionId = String
type SessionData = Map.Map String String
type SessionStore = TVar (Map.Map SessionId SessionData)

createSessionStore :: IO SessionStore
createSessionStore = newTVarIO Map.empty

-- 生成会话ID
generateSessionId :: IO SessionId
generateSessionId = do
    gen <- newStdGen
    return $ take 32 $ randomRs ('a', 'z') gen

-- 会话中间件
sessionMiddleware :: SessionStore -> Middleware
sessionMiddleware store app req respond = do
    sessionId <- case extractSessionId req of
        Just sid -> return sid
        Nothing -> do
            newSid <- generateSessionId
            -- 创建新会话
            atomically $ do
                sessions <- readTVar store
                writeTVar store $ Map.insert newSid Map.empty sessions
            return newSid
    
    let newReq = addSessionToRequest sessionId req
    
    app newReq $ \res -> do
        let cookie = "session=" ++ sessionId ++ "; HttpOnly; Path=/"
            newHeaders = ("Set-Cookie", BS.pack cookie) : responseHeaders res
        respond $ mapResponseHeaders (const newHeaders) res
  where
    extractSessionId req = do
        cookieHeader <- lookup "Cookie" (requestHeaders req)
        -- 简化的cookie解析
        if "session=" `BS.isInfixOf` cookieHeader
            then Just $ extractSessionValue cookieHeader
            else Nothing
    
    extractSessionValue = -- 实现cookie值提取
    addSessionToRequest sid req = -- 将会话ID添加到请求中
```

## 5. CORS 处理中间件

### 基本 CORS 中间件

```haskell
import Network.HTTP.Types
import qualified Data.ByteString.Char8 as BS

-- CORS 配置
data CORSConfig = CORSConfig {
    corsOrigins :: [BS.ByteString],
    corsMethods :: [Method],
    corsHeaders :: [BS.ByteString],
    corsMaxAge :: Int
}

defaultCORSConfig :: CORSConfig
defaultCORSConfig = CORSConfig {
    corsOrigins = ["*"],
    corsMethods = [methodGET, methodPOST, methodPUT, methodDELETE],
    corsHeaders = ["Content-Type", "Authorization"],
    corsMaxAge = 86400
}

-- CORS 中间件
corsMiddleware :: CORSConfig -> Middleware
corsMiddleware config app req respond = do
    if requestMethod req == methodOPTIONS
        then respond $ corsPreflightResponse config req
        else app req (respond . addCORSHeaders config req)

-- 处理预检请求
corsPreflightResponse :: CORSConfig -> Request -> Response
corsPreflightResponse config req = 
    responseLBS status200 headers ""
  where
    headers = buildCORSHeaders config req

-- 添加 CORS 头部
addCORSHeaders :: CORSConfig -> Request -> Response -> Response
addCORSHeaders config req = 
    mapResponseHeaders (++ buildCORSHeaders config req)

-- 构建 CORS 头部
buildCORSHeaders :: CORSConfig -> Request -> [Header]
buildCORSHeaders config req = 
    catMaybes [
        Just ("Access-Control-Allow-Origin", origin),
        Just ("Access-Control-Allow-Methods", methods),
        Just ("Access-Control-Allow-Headers", headers),
        Just ("Access-Control-Max-Age", BS.pack $ show $ corsMaxAge config)
    ]
  where
    origin = case lookup "Origin" (requestHeaders req) of
        Just o | o `elem` corsOrigins config || "*" `elem` corsOrigins config -> o
        _ -> "*"
    
    methods = BS.intercalate ", " $ map (BS.pack . show) $ corsMethods config
    headers = BS.intercalate ", " $ corsHeaders config

-- 使用示例
corsApp :: IO ()
corsApp = scotty 3000 $ do
    middleware $ corsMiddleware defaultCORSConfig
    
    get "/api/data" $ do
        json $ object ["message" .= ("Hello from API!" :: String)]
    
    post "/api/data" $ do
        body <- body
        json $ object ["received" .= body]
```

### 高级 CORS 配置

```haskell
-- 动态 CORS 配置
dynamicCORSMiddleware :: (Request -> CORSConfig) -> Middleware
dynamicCORSMiddleware getConfig app req respond = do
    let config = getConfig req
    corsMiddleware config app req respond

-- 基于路径的 CORS 配置
pathBasedCORSConfig :: Request -> CORSConfig
pathBasedCORSConfig req
    | "/api/" `BS.isInfixOf` rawPathInfo req = apiCORSConfig
    | "/public/" `BS.isInfixOf` rawPathInfo req = publicCORSConfig
    | otherwise = restrictedCORSConfig
  where
    apiCORSConfig = CORSConfig {
        corsOrigins = ["https://app.example.com"],
        corsMethods = [methodGET, methodPOST, methodPUT, methodDELETE],
        corsHeaders = ["Content-Type", "Authorization", "X-API-Key"],
        corsMaxAge = 3600
    }
    
    publicCORSConfig = defaultCORSConfig
    
    restrictedCORSConfig = CORSConfig {
        corsOrigins = [],
        corsMethods = [methodGET],
        corsHeaders = ["Content-Type"],
        corsMaxAge = 300
    }
```

## 完整示例应用

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import qualified Data.Text.Lazy as TL

-- 组合多个中间件的完整应用
fullApp :: IO ()
fullApp = do
    logger <- createFileLogger "app.log"
    sessionStore <- createSessionStore
    
    scotty 3000 $ do
        -- 日志记录
        middleware logStdoutDev
        middleware $ fileLoggingMiddleware logger
        
        -- 安全头部
        middleware securityHeadersMiddleware
        
        -- CORS 支持
        middleware $ corsMiddleware defaultCORSConfig
        
        -- 会话管理
        middleware $ sessionMiddleware sessionStore
        
        -- 路由定义
        get "/" $ do
            text "Welcome to the secure API!"
        
        -- 需要认证的路由
        middleware $ jwtAuthMiddleware ["/", "/login"]
        
        get "/protected" $ do
            json $ object ["message" .= ("This is protected!" :: String)]
        
        post "/login" $ do
            username <- param "username"
            password <- param "password"
            -- 验证用户名密码
            if username == "admin" && password == "secret"
                then do
                    token <- liftIO $ createJWT username
                    case token of
                        Just t -> json $ object ["token" .= t]
                        Nothing -> status status500 
                else status status401

main :: IO ()
main = fullApp
```

## 中间件最佳实践

### 1. 中间件顺序
- 日志记录应该最早执行
- 身份验证在业务逻辑之前
- CORS 处理在认证之前
- 错误处理应该包装其他中间件

### 2. 性能考虑
- 避免在中间件中执行耗时操作
- 使用异步操作处理 I/O
- 合理使用缓存

### 3. 错误处理
- 中间件应该优雅处理异常
- 提供有意义的错误信息
- 记录详细的错误日志

### 4. 可配置性
- 使中间件参数可配置
- 支持环境相关的配置
- 提供合理的默认值

这个中间件系统为 Scotty 应用提供了强大而灵活的横切关注点处理能力，能够有效地分离业务逻辑和基础设施代码。