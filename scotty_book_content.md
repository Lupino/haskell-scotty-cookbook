# Haskell Scotty 网络编程指南
*函数式 Web 开发精要*

---

## 前言

在现代软件开发中，函数式编程正在重新定义我们构建应用程序的方式。Haskell 作为纯函数式编程语言的代表，其强大的类型系统和数学优雅性为 Web 开发带来了新的可能性。Scotty 框架以其简洁的 API 和强大的功能，成为 Haskell Web 开发的理想选择。

本书将带你从零开始，掌握使用 Scotty 构建现代 Web 应用程序的全过程。无论你是从其他编程语言转向 Haskell，还是想要探索函数式 Web 开发的奥秘，这本书都将为你提供系统而实用的指导。

**适合读者：** 具备 Haskell 基础语法知识，了解 HTTP 协议，希望学习函数式 Web 开发的程序员。

---

## 第1章：基础入门

### 1.1 Scotty 框架特点

Scotty 是一个轻量级的 Haskell Web 框架，灵感来源于 Ruby 的 Sinatra。它具有以下特点：

- **简洁的 DSL**：提供直观的路由定义语法
- **类型安全**：利用 Haskell 的类型系统防止运行时错误
- **高性能**：基于 WAI（Web Application Interface）构建
- **易于扩展**：支持中间件和插件系统

```haskell
-- 一个简单的 Scotty 应用示例
{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
    get "/" $ text "Hello, Scotty!"
    get "/hello/:name" $ do
        name <- param "name"
        text $ "Hello, " <> name <> "!"
```

### 1.2 环境配置

#### 安装 Stack

Stack 是 Haskell 的现代构建工具，推荐使用它来管理项目依赖：

```bash
# 在 Ubuntu/Debian 上安装
curl -sSL https://get.haskellstack.org/ | sh

# 在 macOS 上安装
brew install haskell-stack

# 在 Windows 上下载安装器
# https://docs.haskellstack.org/en/stable/install_and_upgrade/
```

#### 创建新项目

使用 stack 创建 scotty 项目

```bash
stack new hello-scotty
cd hello-scotty

```


生成的项目结构如下:

```
hello-scotty/
├── app
│   └── Main.hs          -- 应用入口点
├── CHANGELOG.md
├── LICENSE
├── package.yaml         -- 项目配置
├── README.md
├── Setup.hs
├── src
│   ├── Lib.hs           -- 主要库代码
├── stack.yaml           -- Stack 配置
└── test
    └── Spec.hs          -- 测试文件
```

编辑 package.yaml 添加 scotty 依赖

**package.yaml**
```yaml
dependencies:
- base >= 4.7 && < 5
- scotty
- aeson
- text
```

### 1.3 Hello World 项目

创建你的第一个 Scotty 应用：

**app/Lib.hs**
```haskell
{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( someFunc
  ) where

import           Data.Aeson     (object, (.=))
import           Data.Text.Lazy (Text)
import           Web.Scotty

someFunc :: IO ()
someFunc = do
    putStrLn "Starting server on port 3000..."
    scotty 3000 routes

routes :: ScottyM ()
routes = do
    -- 基本路由
    get "/" $ text "Welcome to Scotty!"

    -- 带参数的路由
    get "/greet/:name" $ do
        name <- param "name"
        text $ "Hello, " <> name <> "!"

    -- JSON 响应
    get "/api/status" $ json $ object
        [ "status" .= ("ok" :: Text)
        , "service" .= ("hello-scotty" :: Text)
        ]
```

运行项目：
```bash
stack build
stack exec hello-scotty-exe
```

运行成功输出如下：

```bash
$ stack exec hello-scotty-exe
Starting server on port 3000...
Setting phasers to stun... (port 3000) (ctrl-c to quit)
```

打开另一个终端，用 curl 进行测试

```bash
$ curl http://127.0.0.1:3000
Welcome to Scotty!

$ curl http://127.0.0.1:3000/greet/Lupino
Hello, Lupino!

$ curl http://127.0.0.1:3000/api/status
{"service":"hello-scotty","status":"ok"}
```

---

## 第2章：路由与请求

### 2.1 路由定义

Scotty 提供了简洁的路由定义语法，支持所有标准 HTTP 方法：

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

routes :: ScottyM ()
routes = do
    -- GET 请求
    get "/users" getAllUsers
    get "/users/:id" getUserById

    -- POST 请求
    post "/users" createUser

    -- PUT 请求
    put "/users/:id" updateUser

    -- DELETE 请求
    delete "/users/:id" deleteUser

    -- 通配符路由
    get "/static/*" serveStatic

    -- 正则表达式路由
    get (regex "^/api/v[0-9]+/users$") apiUsers

getAllUsers :: ActionM ()
getAllUsers = text "All users"

getUserById :: ActionM ()
getUserById = do
    userId <- param "id"
    text $ "User ID: " <> userId
```

### 2.2 参数处理

#### 路径参数

```haskell
-- URL: /users/123/posts/456
get "/users/:userId/posts/:postId" $ do
    userId <- param "userId"
    postId <- param "postId"
    text $ "User: " <> userId <> ", Post: " <> postId
```

#### 查询参数

```haskell
-- URL: /search?q=haskell&limit=10
get "/search" $ do
    query <- param "q"           -- 必需参数
    limit <- param "limit" `rescue` const (return "20")  -- 可选参数
    text $ "Query: " <> query <> ", Limit: " <> limit
```

#### 安全的参数解析

```haskell
import Control.Monad.IO.Class (liftIO)
import Text.Read (readMaybe)

get "/users/:id" $ do
    idText <- param "id"
    case readMaybe idText of
        Nothing -> do
            status status400
            text "Invalid user ID"
        Just userId -> do
            -- 处理有效的用户ID
            text $ "User ID: " <> show (userId :: Int)
```

### 2.3 请求体处理

#### JSON 请求体

```haskell
{-# LANGUAGE DeriveGeneric #-}
import Data.Aeson
import GHC.Generics

data User = User
    { userName :: Text
    , userEmail :: Text
    } deriving (Generic, Show)

instance FromJSON User
instance ToJSON User

post "/users" $ do
    user <- jsonData :: ActionM User
    liftIO $ print user  -- 处理用户数据
    json user  -- 返回创建的用户
```

#### 表单数据

```haskell
post "/login" $ do
    username <- param "username"
    password <- param "password"
    -- 验证逻辑
    if username == "admin" && password == "secret"
        then text "Login successful"
        else do
            status status401
            text "Invalid credentials"
```

### 2.4 文件上传

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Network.Wai.Parse (fileContent, fileName)
import qualified Data.ByteString.Lazy as L

post "/upload" $ do
    files <- files
    case files of
        [] -> text "No file uploaded"
        ((fieldName, fileInfo):_) -> do
            let content = fileContent fileInfo
                name = fileName fileInfo
            liftIO $ L.writeFile ("uploads/" ++ show name) content
            text $ "File uploaded: " <> show name
```

### 2.5 路由组织

将路由分组到不同模块：

**src/Routes/Users.hs**
```haskell
{-# LANGUAGE OverloadedStrings #-}
module Routes.Users (userRoutes) where

import Web.Scotty

userRoutes :: ScottyM ()
userRoutes = do
    get "/users" $ text "All users"
    get "/users/:id" $ do
        userId <- param "id"
        text $ "User: " <> userId
    post "/users" $ text "Create user"
```

**app/Main.hs**
```haskell
import Routes.Users (userRoutes)
import Routes.Posts (postRoutes)

main :: IO ()
main = scotty 3000 $ do
    userRoutes
    postRoutes
```

---

## 第3章：响应与中间件

### 3.1 HTTP 响应构建

#### 状态码设置

```haskell
get "/not-found" $ do
    status status404
    text "Page not found"

post "/users" $ do
    -- 创建用户逻辑
    status status201
    json newUser

delete "/users/:id" $ do
    -- 删除用户逻辑
    status status204
    text ""
```

#### 响应头操作

```haskell
get "/api/data" $ do
    -- 设置缓存头
    setHeader "Cache-Control" "max-age=3600"
    setHeader "Content-Type" "application/json"

    -- 设置 CORS 头
    setHeader "Access-Control-Allow-Origin" "*"
    setHeader "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE"

    json $ object ["data" .= ("some data" :: Text)]
```

### 3.2 JSON 响应

```haskell
{-# LANGUAGE DeriveGeneric #-}
import Data.Aeson

data ApiResponse a = ApiResponse
    { success :: Bool
    , message :: Text
    , result :: Maybe a
    } deriving (Generic, Show)

instance ToJSON a => ToJSON (ApiResponse a)

successResponse :: ToJSON a => a -> ActionM ()
successResponse result = json $ ApiResponse True "Success" (Just result)

errorResponse :: Text -> ActionM ()
errorResponse msg = do
    status status400
    json $ ApiResponse False msg Nothing

get "/api/users/:id" $ do
    userId <- param "id"
    case lookupUser userId of
        Just user -> successResponse user
        Nothing -> errorResponse "User not found"
```

### 3.3 HTML 模板渲染

```haskell
-- 使用 Blaze HTML
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

userPage :: User -> Html
userPage user = docTypeHtml $ do
    H.head $ H.title "User Profile"
    body $ do
        h1 "User Profile"
        p $ toHtml $ "Name: " <> userName user
        p $ toHtml $ "Email: " <> userEmail user

get "/users/:id/profile" $ do
    userId <- param "id"
    case lookupUser userId of
        Just user -> html $ renderHtml $ userPage user
        Nothing -> do
            status status404
            text "User not found"
```

### 3.4 静态文件服务

```haskell
import Network.Wai.Middleware.Static

main :: IO ()
main = scotty 3000 $ do
    -- 静态文件中间件
    middleware $ staticPolicy (noDots >-> addBase "static")

    -- API 路由
    get "/api/hello" $ text "Hello API"

    -- 前端路由（SPA）
    get "/" $ file "static/index.html"
    notFound $ file "static/index.html"  -- 处理前端路由
```

### 3.5 中间件系统

#### 日志中间件

```haskell
import Network.Wai.Middleware.RequestLogger

main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev  -- 开发环境日志
    -- middleware logStdout   -- 生产环境日志

    routes
```

#### 自定义中间件

```haskell
import Network.Wai
import Network.HTTP.Types.Status

-- 请求计时中间件
timingMiddleware :: Middleware
timingMiddleware app req sendResponse = do
    start <- getCurrentTime
    app req $ \response -> do
        end <- getCurrentTime
        let duration = diffUTCTime end start
        putStrLn $ "Request took: " ++ show duration
        sendResponse response

-- 认证中间件
authMiddleware :: Middleware
authMiddleware app req sendResponse = do
    case lookup "Authorization" (requestHeaders req) of
        Nothing -> sendResponse $ responseLBS status401 [] "Unauthorized"
        Just token ->
            if isValidToken token
                then app req sendResponse
                else sendResponse $ responseLBS status401 [] "Invalid token"

main :: IO ()
main = scotty 3000 $ do
    middleware timingMiddleware
    middleware authMiddleware
    routes
```

#### CORS 中间件

```haskell
import Network.Wai.Middleware.Cors

corsPolicy :: CorsResourcePolicy
corsPolicy = simpleCorsResourcePolicy
    { corsOrigins = Nothing  -- 允许所有来源
    , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
    , corsRequestHeaders = ["Content-Type", "Authorization"]
    }

main :: IO ()
main = scotty 3000 $ do
    middleware $ cors (const $ Just corsPolicy)
    routes
```

#### 错误处理中间件

```haskell
-- 全局错误处理
defaultHandler :: ErrorHandler
defaultHandler e = do
    status status500
    json $ object
        [ "error" .= True
        , "message" .= ("Internal server error" :: Text)
        ]

main :: IO ()
main = scotty 3000 $ do
    defaultHandler defaultHandler

    routes
```

---

## 第4章：数据库集成

### 4.1 Persistent 库简介

Persistent 是 Haskell 中最流行的数据库 ORM 库，提供类型安全的数据库操作：

**package.yaml**
```yaml
dependencies:
- persistent
- persistent-sqlite
- persistent-template
- monad-logger
```

### 4.2 数据模型定义

**src/Models.hs**
```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Models where

import Database.Persist.TH
import Data.Text (Text)
import Data.Time

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name Text
    email Text
    createdAt UTCTime default=CURRENT_TIME
    UniqueEmail email
    deriving Show

Post
    title Text
    content Text
    authorId UserId
    createdAt UTCTime default=CURRENT_TIME
    deriving Show

Comment
    content Text
    postId PostId
    authorId UserId
    createdAt UTCTime default=CURRENT_TIME
    deriving Show
|]
```

### 4.3 数据库连接

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Database where

import Database.Persist.Sqlite
import Control.Monad.Logger (runStdoutLoggingT)
import Models

-- 数据库连接池
type DbPool = ConnectionPool

-- 初始化数据库
initDb :: IO DbPool
initDb = runStdoutLoggingT $ do
    pool <- createSqlitePool "app.db" 10
    runSqlPool (runMigration migrateAll) pool
    return pool

-- 运行数据库操作
runDb :: DbPool -> SqlPersistT IO a -> IO a
runDb pool action = runSqlPool action pool
```

### 4.4 CRUD 操作实现

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Controllers.Users where

import Web.Scotty
import Database.Persist
import Database.Persist.Sqlite
import Data.Time
import Models
import Database

-- 获取所有用户
getAllUsersHandler :: DbPool -> ActionM ()
getAllUsersHandler pool = do
    users <- liftIO $ runDb pool $ selectList [] []
    json users

-- 根据ID获取用户
getUserByIdHandler :: DbPool -> ActionM ()
getUserByIdHandler pool = do
    userId <- param "id"
    case fromPathPiece userId of
        Nothing -> do
            status status400
            text "Invalid user ID"
        Just uid -> do
            user <- liftIO $ runDb pool $ get uid
            case user of
                Nothing -> do
                    status status404
                    text "User not found"
                Just u -> json u

-- 创建用户
createUserHandler :: DbPool -> ActionM ()
createUserHandler pool = do
    userData <- jsonData
    now <- liftIO getCurrentTime
    let user = User (userName userData) (userEmail userData) now
    userId <- liftIO $ runDb pool $ insert user
    status status201
    json $ Entity userId user

-- 更新用户
updateUserHandler :: DbPool -> ActionM ()
updateUserHandler pool = do
    userId <- param "id"
    userData <- jsonData
    case fromPathPiece userId of
        Nothing -> do
            status status400
            text "Invalid user ID"
        Just uid -> do
            liftIO $ runDb pool $ update uid
                [ UserName =. userName userData
                , UserEmail =. userEmail userData
                ]
            text "User updated"

-- 删除用户
deleteUserHandler :: DbPool -> ActionM ()
deleteUserHandler pool = do
    userId <- param "id"
    case fromPathPiece userId of
        Nothing -> do
            status status400
            text "Invalid user ID"
        Just uid -> do
            liftIO $ runDb pool $ delete uid
            status status204
            text ""
```

### 4.5 查询构建

```haskell
-- 复杂查询示例
import Database.Persist.Sql

-- 获取用户的所有文章
getUserPosts :: DbPool -> UserId -> IO [Entity Post]
getUserPosts pool userId = runDb pool $
    selectList [PostAuthorId ==. userId] [Asc PostCreatedAt]

-- 获取最近的文章
getRecentPosts :: DbPool -> Int -> IO [Entity Post]
getRecentPosts pool limit = runDb pool $
    selectList [] [Desc PostCreatedAt, LimitTo limit]

-- 搜索文章
searchPosts :: DbPool -> Text -> IO [Entity Post]
searchPosts pool query = runDb pool $ do
    posts <- selectList [] []
    return $ filter (isMatch query) posts
  where
    isMatch q (Entity _ post) =
        q `isInfixOf` postTitle post || q `isInfixOf` postContent post

-- 使用 Esqueleto 进行复杂查询
import Database.Esqueleto.Experimental

-- 获取用户及其文章数量
getUsersWithPostCount :: DbPool -> IO [(Entity User, Value Int)]
getUsersWithPostCount pool = runDb pool $
    select $ do
        (user :& post) <- from $ table @User
            `leftJoin` table @Post
            `on` (\(u :& p) -> u ^. UserId ==. p ^. PostAuthorId)
        groupBy (user ^. UserId)
        return (user, count (post ^. PostId))
```

### 4.6 数据验证

```haskell
-- 数据验证函数
validateUser :: User -> Either Text User
validateUser user
    | T.null (userName user) = Left "Name cannot be empty"
    | not (isValidEmail (userEmail user)) = Left "Invalid email format"
    | otherwise = Right user

isValidEmail :: Text -> Bool
isValidEmail email = "@" `T.isInfixOf` email && "." `T.isInfixOf` email

-- 在控制器中使用验证
createUserHandlerWithValidation :: DbPool -> ActionM ()
createUserHandlerWithValidation pool = do
    userData <- jsonData
    case validateUser userData of
        Left err -> do
            status status400
            json $ object ["error" .= err]
        Right validUser -> do
            userId <- liftIO $ runDb pool $ insert validUser
            status status201
            json $ Entity userId validUser
```

---

## 第5章：认证与安全

### 5.1 JWT Token 认证

首先添加 JWT 相关依赖：

**package.yaml**
```yaml
dependencies:
- jose
- cryptonite
- time
```

JWT 认证实现：

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Auth.JWT where

import Crypto.JOSE
import Crypto.JWT
import Data.Text (Text)
import Data.Time
import Data.Aeson

-- JWT 配置
jwtSecret :: Text
jwtSecret = "your-secret-key-here"

-- 用户声明
data UserClaims = UserClaims
    { userId :: Int
    , userEmail :: Text
    } deriving (Show, Generic)

instance ToJSON UserClaims
instance FromJSON UserClaims

-- 生成 JWT Token
generateToken :: UserClaims -> IO (Either JWTError Text)
generateToken claims = do
    now <- getCurrentTime
    let expiry = addUTCTime 3600 now  -- 1小时过期
        jwt = emptyClaimsSet
            & claimIss ?~ "scotty-app"
            & claimExp ?~ NumericDate expiry
            & claimIat ?~ NumericDate now

    key <- fromSecret <$> pure (encodeUtf8 jwtSecret)
    signedJWT <- runJOSE $ do
        alg <- bestJWSAlg key
        signJWT key (newJWSHeader ((), alg)) jwt

    case signedJWT of
        Left e -> return $ Left e
        Right token -> return $ Right $ decodeUtf8 $ encodeCompact token

-- 验证 JWT Token
verifyToken :: Text -> IO (Either JWTError UserClaims)
verifyToken token = do
    key <- fromSecret <$> pure (encodeUtf8 jwtSecret)
    result <- runJOSE $ do
        jwt <- decodeCompact (encodeUtf8 token)
        verifyJWT (defaultJWTValidationSettings (const True)) key jwt

    case result of
        Left e -> return $ Left e
        Right claims ->
            case fromJSON (toJSON claims) of
                Success userClaims -> return $ Right userClaims
                Error _ -> return $ Left JWTInvalidFormat
```

### 5.2 认证中间件

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Auth.Middleware where

import Web.Scotty
import Network.Wai
import Network.HTTP.Types.Status
import qualified Data.Text as T
import Auth.JWT

-- 认证中间件
authMiddleware :: Middleware
authMiddleware app req sendResponse = do
    case getAuthToken req of
        Nothing -> unauthorized
        Just token -> do
            result <- verifyToken token
            case result of
                Left _ -> unauthorized
                Right _ -> app req sendResponse
  where
    unauthorized = sendResponse $ responseLBS status401 [] "Unauthorized"

-- 从请求头获取认证令牌
getAuthToken :: Request -> Maybe Text
getAuthToken req = do
    authHeader <- lookup "Authorization" (requestHeaders req)
    let headerText = decodeUtf8 authHeader
    if "Bearer " `T.isPrefixOf` headerText
        then Just $ T.drop 7 headerText
        else Nothing

-- Scotty 认证助手
requireAuth :: ActionM UserClaims
requireAuth = do
    authHeader <- header "Authorization"
    case authHeader of
        Nothing -> do
            status status401
            json $ object ["error" .= ("Missing authorization header" :: Text)]
            finish
        Just headerValue -> do
            let token = T.drop 7 headerValue  -- 移除 "Bearer "
            result <- liftIO $ verifyToken token
            case result of
                Left _ -> do
                    status status401
                    json $ object ["error" .= ("Invalid token" :: Text)]
                    finish
                Right claims -> return claims
```

### 5.3 用户认证流程

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Controllers.Auth where

import Web.Scotty
import Database.Persist
import Crypto.BCrypt
import Models
import Database
import Auth.JWT

data LoginRequest = LoginRequest
    { loginEmail :: Text
    , loginPassword :: Text
    } deriving (Generic, Show)

instance FromJSON LoginRequest

-- 用户注册
registerHandler :: DbPool -> ActionM ()
registerHandler pool = do
    userData <- jsonData :: ActionM LoginRequest

    -- 检查用户是否已存在
    existingUser <- liftIO $ runDb pool $
        selectFirst [UserEmail ==. loginEmail userData] []

    case existingUser of
        Just _ -> do
            status status409
            json $ object ["error" .= ("User already exists" :: Text)]
        Nothing -> do
            -- 密码哈希
            hashedPassword <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy
                (encodeUtf8 $ loginPassword userData)

            case hashedPassword of
                Nothing -> do
                    status status500
                    json $ object ["error" .= ("Password hashing failed" :: Text)]
                Just hash -> do
                    now <- liftIO getCurrentTime
                    let newUser = User
                            (loginEmail userData)
                            (decodeUtf8 hash)
                            now

                    userId <- liftIO $ runDb pool $ insert newUser

                    -- 生成 JWT
                    let claims = UserClaims (fromSqlKey userId) (loginEmail userData)
                    tokenResult <- liftIO $ generateToken claims

                    case tokenResult of
                        Left _ -> do
                            status status500
                            json $ object ["error" .= ("Token generation failed" :: Text)]
                        Right token -> do
                            status status201
                            json $ object
                                [ "token" .= token
                                , "user" .= object
                                    [ "id" .= fromSqlKey userId
                                    , "email" .= loginEmail userData
                                    ]
                                ]

-- 用户登录
loginHandler :: DbPool -> ActionM ()
loginHandler pool = do
    loginData <- jsonData :: ActionM LoginRequest

    -- 查找用户
    maybeUser <- liftIO $ runDb pool $
        selectFirst [UserEmail ==. loginEmail loginData] []

    case maybeUser of
        Nothing -> do
            status status401
            json $ object ["error" .= ("Invalid credentials" :: Text)]
        Just (Entity userId user) -> do
            -- 验证密码
            let isValid = validatePassword
                    (encodeUtf8 $ userPassword user)
                    (encodeUtf8 $ loginPassword loginData)

            if isValid
                then do
                    -- 生成 JWT
                    let claims = UserClaims (fromSqlKey userId) (userEmail user)
                    tokenResult <- liftIO $ generateToken claims

                    case tokenResult of
                        Left _ -> do
                            status status500
                            json $ object ["error" .= ("Token generation failed" :: Text)]
                        Right token -> do
                            json $ object
                                [ "token" .= token
                                , "user" .= object
                                    [ "id" .= fromSqlKey userId
                                    , "email" .= userEmail user
                                    ]
                                ]
                else do
                    status status401
                    json $ object ["error" .= ("Invalid credentials" :: Text)]

-- 获取当前用户信息
profileHandler :: ActionM ()
profileHandler = do
    claims <- requireAuth
    json $ object
        [ "id" .= userId claims
        , "email" .= userEmail claims
        ]
```

### 5.4 Session 管理

对于某些场景，你可能需要基于 Session 的认证：

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Auth.Session where

import Web.Scotty
import Web.ClientSession
import qualified Data.Map as Map
import Data.IORef
import Control.Concurrent.STM

-- 简单的内存 Session 存储
type SessionStore = TVar (Map.Map Text UserClaims)

-- 初始化 Session 存储
initSessionStore :: IO SessionStore
initSessionStore = newTVarIO Map.empty

-- 创建 Session
createSession :: SessionStore -> UserClaims -> IO Text
createSession store claims = do
    sessionId <- generateSessionId
    atomically $ modifyTVar store (Map.insert sessionId claims)
    return sessionId

-- 获取 Session
getSession :: SessionStore -> Text -> IO (Maybe UserClaims)
getSession store sessionId = do
    sessions <- readTVarIO store
    return $ Map.lookup sessionId sessions

-- 删除 Session
deleteSession :: SessionStore -> Text -> IO ()
deleteSession store sessionId =
    atomically $ modifyTVar store (Map.delete sessionId)

-- 生成 Session ID
generateSessionId :: IO Text
generateSessionId = do
    -- 实现随机 Session ID 生成
    -- 这里简化处理
    return "session-123"
```

### 5.5 安全最佳实践

```haskell
-- HTTPS 重定向中间件
httpsRedirectMiddleware :: Middleware
httpsRedirectMiddleware app req sendResponse = do
    if isSecure req
        then app req sendResponse
        else do
            let location = "https://" <> requestHost req <> rawPathInfo req <> rawQueryString req
            sendResponse $ responseLBS status301
                [("Location", location)]
                "Moved Permanently"

-- 安全头中间件
securityHeadersMiddleware :: Middleware
securityHeadersMiddleware app req sendResponse =
    app req $ \response -> do
        let headers =
                [ ("X-Content-Type-Options", "nosniff")
                , ("X-Frame-Options", "DENY")
                , ("X-XSS-Protection", "1; mode=block")
                , ("Strict-Transport-Security", "max-age=31536000; includeSubDomains")
                , ("Content-Security-Policy", "default-src 'self'")
                ]
        sendResponse $ mapResponseHeaders (++ headers) response

-- 速率限制
import Data.Time
import qualified Data.Map as Map
import Control.Concurrent.STM

type RateLimitStore = TVar (Map.Map Text (UTCTime, Int))

rateLimitMiddleware :: Int -> NominalDiffTime -> RateLimitStore -> Middleware
rateLimitMiddleware maxRequests window store app req sendResponse = do
    now <- getCurrentTime
    let clientIP = show $ remoteHost req

    allowed <- atomically $ do
        limitMap <- readTVar store
        case Map.lookup clientIP limitMap of
            Nothing -> do
                writeTVar store $ Map.insert clientIP (now, 1) limitMap
                return True
            Just (lastTime, count) -> do
                if diffUTCTime now lastTime > window
                    then do
                        writeTVar store $ Map.insert clientIP (now, 1) limitMap
                        return True
                    else if count >= maxRequests
                        then return False
                        else do
                            writeTVar store $ Map.insert clientIP (lastTime, count + 1) limitMap
                            return True

    if allowed
        then app req sendResponse
        else sendResponse $ responseLBS status429 [] "Too Many Requests"

-- 使用安全中间件
main :: IO ()
main = do
    rateLimitStore <- newTVarIO Map.empty
    scotty 3000 $ do
        middleware $ rateLimitMiddleware 100 3600 rateLimitStore  -- 每小时100次请求
        middleware securityHeadersMiddleware
        middleware httpsRedirectMiddleware
        routes
```

---

## 第6章：部署与测试

### 6.1 配置管理

创建灵活的配置系统：

**src/Config.hs**
```haskell
{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.Text (Text)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data Config = Config
    { configPort :: Int
    , configDbPath :: Text
    , configJwtSecret :: Text
    , configLogLevel :: LogLevel
    } deriving Show

data LogLevel = Debug | Info | Warning | Error
    deriving (Show, Read, Eq)

-- 默认配置
defaultConfig :: Config
defaultConfig = Config
    { configPort = 3000
    , configDbPath = "app.db"
    , configJwtSecret = "default-secret-change-in-production"
    , configLogLevel = Info
    }

-- 从环境变量加载配置
loadConfig :: IO Config
loadConfig = do
    port <- maybe (configPort defaultConfig) id <$>
        (readMaybe =<< lookupEnv "PORT")

    dbPath <- maybe (configDbPath defaultConfig) fromString <$>
        lookupEnv "DATABASE_URL"

    jwtSecret <- maybe (configJwtSecret defaultConfig) fromString <$>
        lookupEnv "JWT_SECRET"

    logLevel <- maybe (configLogLevel defaultConfig) id <$>
        (readMaybe =<< lookupEnv "LOG_LEVEL")

    return Config
        { configPort = port
        , configDbPath = dbPath
        , configJwtSecret = jwtSecret
        , configLogLevel = logLevel
        }

-- 配置验证
validateConfig :: Config -> Either Text Config
validateConfig config
    | configPort config <= 0 = Left "Port must be positive"
    | T.null (configJwtSecret config) = Left "JWT secret cannot be empty"
    | T.length (configJwtSecret config) < 32 = Left "JWT secret too short"
    | otherwise = Right config
```

### 6.2 环境管理

**config/development.yaml**
```yaml
port: 3000
database_url: "app_dev.db"
jwt_secret: "development-secret-key"
log_level: "Debug"
```

**config/production.yaml**
```yaml
port: 8080
database_url: "app_prod.db"
jwt_secret: "${JWT_SECRET}"
log_level: "Warning"
```

### 6.3 Docker 容器化

**Dockerfile**
```dockerfile
# 多阶段构建
FROM haskell:8.10 as builder

WORKDIR /app
COPY stack.yaml package.yaml ./
COPY src/ src/
COPY app/ app/

RUN stack setup
RUN stack build --only-dependencies
RUN stack build

# 运行时镜像
FROM ubuntu:20.04

RUN apt-get update && apt-get install -y \
    ca-certificates \
    libgmp10 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=builder /app/.stack-work/install/x86_64-linux/*/8.10.*/bin/scotty-app .
COPY static/ static/

EXPOSE 8080

CMD ["./scotty-app"]
```

**docker-compose.yml**
```yaml
version: '3.8'

services:
  app:
    build: .
    ports:
      - "8080:8080"
    environment:
      - PORT=8080
      - DATABASE_URL=postgresql://user:pass@db:5432/scotty_app
      - JWT_SECRET=${JWT_SECRET}
    depends_on:
      - db
    volumes:
      - ./uploads:/app/uploads

  db:
    image: postgres:13
    environment:
      - POSTGRES_DB=scotty_app
      - POSTGRES_USER=user
      - POSTGRES_PASSWORD=pass
    volumes:
      - postgres_data:/var/lib/postgresql/data
    ports:
      - "5432:5432"

  nginx:
    image: nginx:alpine
    ports:
      - "80:80"
      - "443:443"
    volumes:
      - ./nginx.conf:/etc/nginx/nginx.conf
      - ./ssl:/etc/nginx/ssl
    depends_on:
      - app

volumes:
  postgres_data:
```

### 6.4 单元测试

**package.yaml**
```yaml
dependencies:
- hspec
- hspec-wai
- hspec-wai-json

tests:
  scotty-app-test:
    main: Spec.hs
    source-dirs: test
    dependencies:
    - scotty-app
```

**test/Spec.hs**
```haskell
{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Web.Scotty
import Network.Wai (Application)
import Models
import Controllers.Users

-- 测试应用
testApp :: IO Application
testApp = do
    pool <- initTestDb
    scottyApp $ do
        get "/users" $ getAllUsersHandler pool
        post "/users" $ createUserHandler pool

-- 测试数据库初始化
initTestDb :: IO DbPool
initTestDb = runStdoutLoggingT $ do
    pool <- createSqlitePool ":memory:" 1
    runSqlPool (runMigration migrateAll) pool
    return pool

-- 测试用例
spec :: Spec
spec = with testApp $ do
    describe "GET /users" $ do
        it "returns empty list initially" $ do
            get "/users" `shouldRespondWith` "[]"

        it "responds with 200" $ do
            get "/users" `shouldRespondWith` 200

    describe "POST /users" $ do
        it "creates a new user" $ do
            post "/users" [json|{name: "John", email: "john@example.com"}|]
                `shouldRespondWith` 201

        it "validates user data" $ do
            post "/users" [json|{name: "", email: "invalid"}|]
                `shouldRespondWith` 400

main :: IO ()
main = hspec spec
```

### 6.5 API 测试

**test/ApiSpec.hs**
```haskell
{-# LANGUAGE OverloadedStrings #-}
module ApiSpec where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Data.Aeson

-- API 集成测试
apiSpec :: Spec
apiSpec = with testApp $ do
    describe "User API" $ do
        it "creates and retrieves user" $ do
            -- 创建用户
            post "/users" [json|{name: "Alice", email: "alice@example.com"}|]
                `shouldRespondWith` 201

            -- 获取用户列表
            get "/users" `shouldRespondWith`
                [json|[{name: "Alice", email: "alice@example.com"}]|]

        it "handles authentication" $ do
            -- 注册用户
            post "/auth/register"
                [json|{email: "test@example.com", password: "password123"}|]
                `shouldRespondWith` 201

            -- 登录获取 token
            response <- post "/auth/login"
                [json|{email: "test@example.com", password: "password123"}|]

            -- 使用 token 访问受保护路由
            let token = extractToken response
            request "GET" "/profile"
                [("Authorization", "Bearer " <> token)] ""
                `shouldRespondWith` 200

extractToken :: WaiSession st SResponse -> Text
extractToken response =
    -- 从响应中提取 JWT token
    -- 实现细节省略
    "extracted-token"
```

### 6.6 性能测试

使用 criterion 进行性能测试：

**test/BenchSpec.hs**
```haskell
{-# LANGUAGE OverloadedStrings #-}
module BenchSpec where

import Criterion.Main
import Network.Wai.Test
import Network.HTTP.Types

-- 性能基准测试
benchmarks :: [Benchmark]
benchmarks =
    [ bench "GET /users" $ whnfIO $ do
        app <- testApp
        runSession (request "GET" "/users" [] "") app

    , bench "POST /users" $ whnfIO $ do
        app <- testApp
        let payload = [json|{name: "Test", email: "test@example.com"}|]
        runSession (request "POST" "/users" [] payload) app
    ]

main :: IO ()
main = defaultMain benchmarks
```

### 6.7 持续集成

**.github/workflows/ci.yml**
```yaml
name: CI

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest

    services:
      postgres:
        image: postgres:13
        env:
          POSTGRES_PASSWORD: postgres
        options: >-
          --health-cmd pg_isready
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5

    steps:
    - uses: actions/checkout@v2

    - name: Setup Haskell
      uses: haskell/actions/setup@v1
      with:
        ghc-version: '8.10'
        stack-version: 'latest'

    - name: Cache dependencies
      uses: actions/cache@v2
      with:
        path: ~/.stack
        key: ${{ runner.os }}-stack-${{ hashFiles('stack.yaml') }}

    - name: Install dependencies
      run: stack build --only-dependencies --test

    - name: Build
      run: stack build --test --no-run-tests

    - name: Run tests
      run: stack test
      env:
        DATABASE_URL: postgresql://postgres:postgres@localhost:5432/test

    - name: Run benchmarks
      run: stack bench

  deploy:
    needs: test
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/main'

    steps:
    - uses: actions/checkout@v2

    - name: Build Docker image
      run: docker build -t scotty-app .

    - name: Deploy to production
      # 部署脚本
      run: echo "Deploying to production"
```

---

## 第7章：实战项目 - 博客系统

### 7.1 项目架构设计

我们将构建一个完整的博客系统，包含以下功能：
- 用户注册和登录
- 文章创建、编辑、删除
- 评论系统
- 标签管理
- 文章搜索

项目结构：
```
blog-system/
├── src/
│   ├── Models/
│   │   ├── User.hs
│   │   ├── Post.hs
│   │   └── Comment.hs
│   ├── Controllers/
│   │   ├── Auth.hs
│   │   ├── Posts.hs
│   │   └── Comments.hs
│   ├── Services/
│   │   ├── PostService.hs
│   │   └── UserService.hs
│   ├── Utils/
│   │   └── Validation.hs
│   └── Config.hs
├── static/
│   ├── css/
│   ├── js/
│   └── index.html
└── templates/
```

### 7.2 数据模型设计

**src/Models/Blog.hs**
```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Models.Blog where

import Database.Persist.TH
import Data.Text (Text)
import Data.Time

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    email Text
    passwordHash Text
    displayName Text
    bio Text Maybe
    createdAt UTCTime default=CURRENT_TIME
    updatedAt UTCTime default=CURRENT_TIME
    UniqueEmail email
    deriving Show Eq

Post
    title Text
    slug Text
    content Text
    excerpt Text Maybe
    authorId UserId
    published Bool default=False
    publishedAt UTCTime Maybe
    createdAt UTCTime default=CURRENT_TIME
    updatedAt UTCTime default=CURRENT_TIME
    UniqueSlug slug
    deriving Show Eq

Tag
    name Text
    slug Text
    createdAt UTCTime default=CURRENT_TIME
    UniqueName name
    UniqueTagSlug slug
    deriving Show Eq

PostTag
    postId PostId
    tagId TagId
    UniquePostTag postId tagId
    deriving Show Eq

Comment
    content Text
    postId PostId
    authorId UserId
    parentId CommentId Maybe
    createdAt UTCTime default=CURRENT_TIME
    updatedAt UTCTime default=CURRENT_TIME
    deriving Show Eq
|]

-- 数据传输对象
data PostDTO = PostDTO
    { postDtoTitle :: Text
    , postDtoContent :: Text
    , postDtoExcerpt :: Maybe Text
    , postDtoTags :: [Text]
    , postDtoPublished :: Bool
    } deriving (Show, Generic)

instance FromJSON PostDTO where
    parseJSON = withObject "PostDTO" $ \o -> PostDTO
        <$> o .: "title"
        <*> o .: "content"
        <*> o .:? "excerpt"
        <*> o .:? "tags" .!= []
        <*> o .:? "published" .!= False

instance ToJSON PostDTO where
    toJSON (PostDTO title content excerpt tags published) = object
        [ "title" .= title
        , "content" .= content
        , "excerpt" .= excerpt
        , "tags" .= tags
        , "published" .= published
        ]
```

### 7.3 服务层实现

**src/Services/PostService.hs**
```haskell
{-# LANGUAGE OverloadedStrings #-}
module Services.PostService where

import Database.Persist
import Database.Persist.Sql
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Models.Blog
import Database

-- 文章服务
data PostService = PostService
    { createPost :: UserId -> PostDTO -> IO (Either Text (Entity Post))
    , updatePost :: PostId -> UserId -> PostDTO -> IO (Either Text (Entity Post))
    , deletePost :: PostId -> UserId -> IO (Either Text ())
    , getPost :: PostId -> IO (Maybe (Entity Post))
    , getPostBySlug :: Text -> IO (Maybe (Entity Post))
    , getUserPosts :: UserId -> Int -> Int -> IO [Entity Post]
    , getPublishedPosts :: Int -> Int -> IO [Entity Post]
    , searchPosts :: Text -> Int -> Int -> IO [Entity Post]
    }

-- 创建文章服务实例
mkPostService :: DbPool -> PostService
mkPostService pool = PostService
    { createPost = createPostImpl pool
    , updatePost = updatePostImpl pool
    , deletePost = deletePostImpl pool
    , getPost = getPostImpl pool
    , getPostBySlug = getPostBySlugImpl pool
    , getUserPosts = getUserPostsImpl pool
    , getPublishedPosts = getPublishedPostsImpl pool
    , searchPosts = searchPostsImpl pool
    }

-- 实现函数
createPostImpl :: DbPool -> UserId -> PostDTO -> IO (Either Text (Entity Post))
createPostImpl pool authorId postDto = do
    now <- getCurrentTime
    slug <- generateSlug (postDtoTitle postDto)

    -- 检查 slug 是否已存在
    existingPost <- runDb pool $ selectFirst [PostSlug ==. slug] []
    case existingPost of
        Just _ -> return $ Left "Post with this title already exists"
        Nothing -> do
            let post = Post
                    { postTitle = postDtoTitle postDto
                    , postSlug = slug
                    , postContent = postDtoContent postDto
                    , postExcerpt = postDtoExcerpt postDto
                    , postAuthorId = authorId
                    , postPublished = postDtoPublished postDto
                    , postPublishedAt = if postDtoPublished postDto then Just now else Nothing
                    , postCreatedAt = now
                    , postUpdatedAt = now
                    }

            postId <- runDb pool $ insert post

            -- 处理标签
            mapM_ (createOrAssignTag pool postId) (postDtoTags postDto)

            return $ Right $ Entity postId post

updatePostImpl :: DbPool -> PostId -> UserId -> PostDTO -> IO (Either Text (Entity Post))
updatePostImpl pool postId userId postDto = do
    maybePost <- runDb pool $ get postId
    case maybePost of
        Nothing -> return $ Left "Post not found"
        Just post ->
            if postAuthorId post /= userId
                then return $ Left "Unauthorized"
                else do
                    now <- getCurrentTime
                    slug <- if postTitle post /= postDtoTitle postDto
                            then generateSlug (postDtoTitle postDto)
                            else return (postSlug post)

                    let updatedPost = post
                            { postTitle = postDtoTitle postDto
                            , postSlug = slug
                            , postContent = postDtoContent postDto
                            , postExcerpt = postDtoExcerpt postDto
                            , postPublished = postDtoPublished postDto
                            , postPublishedAt = if postDtoPublished postDto && not (postPublished post)
                                               then Just now
                                               else postPublishedAt post
                            , postUpdatedAt = now
                            }

                    runDb pool $ replace postId updatedPost

                    -- 更新标签
                    runDb pool $ deleteWhere [PostTagPostId ==. postId]
                    mapM_ (createOrAssignTag pool postId) (postDtoTags postDto)

                    return $ Right $ Entity postId updatedPost

-- 生成 URL slug
generateSlug :: Text -> IO Text
generateSlug title = do
    let baseSlug = T.toLower $ T.map replaceChar $ T.strip title
        replaceChar c
            | c `elem` [' ', '\t', '\n'] = '-'
            | c `elem` ['/', '\\', '?', '#', '[', ']', '@'] = '-'
            | otherwise = c
    return $ T.take 100 baseSlug

-- 创建或分配标签
createOrAssignTag :: DbPool -> PostId -> Text -> IO ()
createOrAssignTag pool postId tagName = do
    now <- getCurrentTime
    tagSlug <- generateSlug tagName

    -- 查找或创建标签
    tagId <- runDb pool $ do
        maybeTag <- selectFirst [TagName ==. tagName] []
        case maybeTag of
            Just (Entity tId _) -> return tId
            Nothing -> insert $ Tag tagName tagSlug now

    -- 关联文章和标签
    runDb pool $ insertUnique $ PostTag postId tagId
    return ()

-- 其他实现函数
getPostImpl :: DbPool -> PostId -> IO (Maybe (Entity Post))
getPostImpl pool postId = runDb pool $ selectFirst [PostId ==. postId] []

getPostBySlugImpl :: DbPool -> Text -> IO (Maybe (Entity Post))
getPostBySlugImpl pool slug = runDb pool $ selectFirst [PostSlug ==. slug] []

getUserPostsImpl :: DbPool -> UserId -> Int -> Int -> IO [Entity Post]
getUserPostsImpl pool userId offset limit = runDb pool $
    selectList [PostAuthorId ==. userId]
               [Desc PostCreatedAt, OffsetBy offset, LimitTo limit]

getPublishedPostsImpl :: DbPool -> Int -> Int -> IO [Entity Post]
getPublishedPostsImpl pool offset limit = runDb pool $
    selectList [PostPublished ==. True]
               [Desc PostPublishedAt, OffsetBy offset, LimitTo limit]

searchPostsImpl :: DbPool -> Text -> Int -> Int -> IO [Entity Post]
searchPostsImpl pool query offset limit = runDb pool $ do
    posts <- selectList [PostPublished ==. True]
                        [Desc PostPublishedAt, OffsetBy offset, LimitTo limit]
    return $ filter (matchesQuery query) posts
  where
    matchesQuery q (Entity _ post) =
        q `T.isInfixOf` T.toLower (postTitle post) ||
        q `T.isInfixOf` T.toLower (postContent post)
```

### 7.4 控制器实现

**src/Controllers/Posts.hs**
```haskell
{-# LANGUAGE OverloadedStrings #-}
module Controllers.Posts where

import Web.Scotty
import Database.Persist
import Models.Blog
import Services.PostService
import Auth.Middleware
import Control.Monad.IO.Class

-- 文章控制器
postRoutes :: PostService -> DbPool -> ScottyM ()
postRoutes postService pool = do
    -- 获取已发布文章列表
    get "/api/posts" $ do
        page <- param "page" `rescue` const (return "1")
        limit <- param "limit" `rescue` const (return "10")

        let pageNum = max 1 (read page)
            limitNum = min 50 (max 1 (read limit))
            offset = (pageNum - 1) * limitNum

        posts <- liftIO $ getPublishedPosts postService offset limitNum
        json posts

    -- 根据 slug 获取文章
    get "/api/posts/:slug" $ do
        slug <- param "slug"
        maybePost <- liftIO $ getPostBySlug postService slug
        case maybePost of
            Nothing -> do
                status status404
                json $ object ["error" .= ("Post not found" :: Text)]
            Just post -> json post

    -- 搜索文章
    get "/api/posts/search/:query" $ do
        query <- param "query"
        page <- param "page" `rescue` const (return "1")
        limit <- param "limit" `rescue` const (return "10")

        let pageNum = max 1 (read page)
            limitNum = min 50 (max 1 (read limit))
            offset = (pageNum - 1) * limitNum

        posts <- liftIO $ searchPosts postService query offset limitNum
        json posts

    -- 创建文章（需要认证）
    post "/api/posts" $ do
        claims <- requireAuth
        postDto <- jsonData :: ActionM PostDTO

        result <- liftIO $ createPost postService (toSqlKey $ userId claims) postDto
        case result of
            Left err -> do
                status status400
                json $ object ["error" .= err]
            Right post -> do
                status status201
                json post

    -- 更新文章（需要认证）
    put "/api/posts/:id" $ do
        claims <- requireAuth
        postIdText <- param "id"
        postDto <- jsonData :: ActionM PostDTO

        case fromPathPiece postIdText of
            Nothing -> do
                status status400
                json $ object ["error" .= ("Invalid post ID" :: Text)]
            Just postId -> do
                result <- liftIO $ updatePost postService postId
                    (toSqlKey $ userId claims) postDto
                case result of
                    Left err -> do
                        status status400
                        json $ object ["error" .= err]
                    Right post -> json post

    -- 删除文章（需要认证）
    delete "/api/posts/:id" $ do
        claims <- requireAuth
        postIdText <- param "id"

        case fromPathPiece postIdText of
            Nothing -> do
                status status400
                json $ object ["error" .= ("Invalid post ID" :: Text)]
            Just postId -> do
                result <- liftIO $ deletePost postService postId
                    (toSqlKey $ userId claims)
                case result of
                    Left err -> do
                        status status400
                        json $ object ["error" .= err]
                    Right _ -> do
                        status status204
                        text ""

    -- 获取用户的文章列表（需要认证）
    get "/api/my-posts" $ do
        claims <- requireAuth
        page <- param "page" `rescue` const (return "1")
        limit <- param "limit" `rescue` const (return "10")

        let pageNum = max 1 (read page)
            limitNum = min 50 (max 1 (read limit))
            offset = (pageNum - 1) * limitNum

        posts <- liftIO $ getUserPosts postService
            (toSqlKey $ userId claims) offset limitNum
        json posts
```

### 7.5 前端集成

**static/index.html**
```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Haskell Blog</title>
    <link href="https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css" rel="stylesheet">
    <script src="https://unpkg.com/vue@3/dist/vue.global.js"></script>
    <script src="https://unpkg.com/axios/dist/axios.min.js"></script>
</head>
<body>
    <div id="app">
        <nav class="bg-blue-600 text-white p-4">
            <div class="container mx-auto flex justify-between items-center">
                <h1 class="text-xl font-bold">Haskell Blog</h1>
                <div>
                    <button v-if="!user" @click="showLogin = true"
                            class="bg-blue-800 px-4 py-2 rounded mr-2">Login</button>
                    <button v-if="!user" @click="showRegister = true"
                            class="bg-green-600 px-4 py-2 rounded">Register</button>
                    <span v-if="user" class="mr-4">Welcome, {{ user.email }}</span>
                    <button v-if="user" @click="logout"
                            class="bg-red-600 px-4 py-2 rounded">Logout</button>
                </div>
            </div>
        </nav>

        <main class="container mx-auto mt-8 px-4">
            <!-- 文章列表 -->
            <div v-if="currentView === 'posts'">
                <h2 class="text-2xl font-bold mb-4">Recent Posts</h2>
                <div v-for="post in posts" :key="post.id"
                     class="bg-white shadow rounded-lg p-6 mb-4">
                    <h3 class="text-xl font-semibold mb-2">{{ post.title }}</h3>
                    <p class="text-gray-600 mb-4">{{ post.excerpt || post.content.substring(0, 200) + '...' }}</p>
                    <button @click="viewPost(post)"
                            class="bg-blue-600 text-white px-4 py-2 rounded">Read More</button>
                </div>
            </div>

            <!-- 文章详情 -->
            <div v-if="currentView === 'post' && selectedPost">
                <article class="bg-white shadow rounded-lg p-6">
                    <h1 class="text-3xl font-bold mb-4">{{ selectedPost.title }}</h1>
                    <div class="prose max-w-none" v-html="selectedPost.content"></div>
                </article>
                <button @click="currentView = 'posts'"
                        class="mt-4 bg-gray-600 text-white px-4 py-2 rounded">Back to Posts</button>

                <!-- 评论系统 -->
                <div class="mt-8">
                    <h3 class="text-xl font-bold mb-4">Comments</h3>
                    <div v-if="user" class="mb-4">
                        <textarea v-model="newComment"
                                placeholder="Write a comment..."
                                class="w-full p-3 border rounded"></textarea>
                        <button @click="addComment"
                                class="mt-2 bg-blue-600 text-white px-4 py-2 rounded">Post Comment</button>
                    </div>
                    <div v-for="comment in comments" :key="comment.id"
                         class="bg-gray-50 p-4 rounded mb-2">
                        <p>{{ comment.content }}</p>
                        <small class="text-gray-500">{{ formatDate(comment.createdAt) }}</small>
                    </div>
                </div>
            </div>

            <!-- 我的文章（用户登录后） -->
            <div v-if="currentView === 'my-posts' && user">
                <div class="flex justify-between items-center mb-4">
                    <h2 class="text-2xl font-bold">My Posts</h2>
                    <button @click="showPostEditor = true"
                            class="bg-green-600 text-white px-4 py-2 rounded">New Post</button>
                </div>
                <div v-for="post in myPosts" :key="post.id"
                     class="bg-white shadow rounded-lg p-4 mb-4">
                    <h3 class="text-lg font-semibold">{{ post.title }}</h3>
                    <div class="flex mt-2">
                        <button @click="editPost(post)"
                                class="bg-blue-600 text-white px-3 py-1 rounded mr-2">Edit</button>
                        <button @click="deletePost(post.id)"
                                class="bg-red-600 text-white px-3 py-1 rounded">Delete</button>
                    </div>
                </div>
            </div>
        </main>

        <!-- 登录模态框 -->
        <div v-if="showLogin" class="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center">
            <div class="bg-white p-8 rounded-lg w-96">
                <h2 class="text-xl font-bold mb-4">Login</h2>
                <input v-model="loginForm.email" type="email" placeholder="Email"
                       class="w-full p-2 border rounded mb-4">
                <input v-model="loginForm.password" type="password" placeholder="Password"
                       class="w-full p-2 border rounded mb-4">
                <div class="flex justify-end">
                    <button @click="showLogin = false"
                            class="bg-gray-500 text-white px-4 py-2 rounded mr-2">Cancel</button>
                    <button @click="login"
                            class="bg-blue-600 text-white px-4 py-2 rounded">Login</button>
                </div>
            </div>
        </div>

        <!-- 注册模态框 -->
        <div v-if="showRegister" class="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center">
            <div class="bg-white p-8 rounded-lg w-96">
                <h2 class="text-xl font-bold mb-4">Register</h2>
                <input v-model="registerForm.email" type="email" placeholder="Email"
                       class="w-full p-2 border rounded mb-4">
                <input v-model="registerForm.displayName" type="text" placeholder="Display Name"
                       class="w-full p-2 border rounded mb-4">
                <input v-model="registerForm.password" type="password" placeholder="Password"
                       class="w-full p-2 border rounded mb-4">
                <div class="flex justify-end">
                    <button @click="showRegister = false"
                            class="bg-gray-500 text-white px-4 py-2 rounded mr-2">Cancel</button>
                    <button @click="register"
                            class="bg-green-600 text-white px-4 py-2 rounded">Register</button>
                </div>
            </div>
        </div>

        <!-- 文章编辑器 -->
        <div v-if="showPostEditor" class="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center">
            <div class="bg-white p-8 rounded-lg w-4/5 h-4/5 overflow-y-auto">
                <h2 class="text-xl font-bold mb-4">{{ editingPost ? 'Edit Post' : 'New Post' }}</h2>
                <input v-model="postForm.title" type="text" placeholder="Post Title"
                       class="w-full p-2 border rounded mb-4">
                <textarea v-model="postForm.content" placeholder="Post Content"
                          class="w-full p-2 border rounded mb-4 h-64"></textarea>
                <input v-model="postForm.excerpt" type="text" placeholder="Excerpt (optional)"
                       class="w-full p-2 border rounded mb-4">
                <input v-model="postForm.tags" type="text" placeholder="Tags (comma separated)"
                       class="w-full p-2 border rounded mb-4">
                <div class="mb-4">
                    <label class="flex items-center">
                        <input v-model="postForm.published" type="checkbox" class="mr-2">
                        Publish immediately
                    </label>
                </div>
                <div class="flex justify-end">
                    <button @click="closePostEditor"
                            class="bg-gray-500 text-white px-4 py-2 rounded mr-2">Cancel</button>
                    <button @click="savePost"
                            class="bg-blue-600 text-white px-4 py-2 rounded">Save</button>
                </div>
            </div>
        </div>
    </div>

    <script>
        const { createApp } = Vue;

        createApp({
            data() {
                return {
                    user: null,
                    posts: [],
                    myPosts: [],
                    selectedPost: null,
                    comments: [],
                    currentView: 'posts',
                    showLogin: false,
                    showRegister: false,
                    showPostEditor: false,
                    editingPost: null,
                    newComment: '',
                    loginForm: { email: '', password: '' },
                    registerForm: { email: '', displayName: '', password: '' },
                    postForm: { title: '', content: '', excerpt: '', tags: '', published: false }
                }
            },
            async mounted() {
                this.checkAuth();
                await this.loadPosts();
            },
            methods: {
                // 认证相关
                checkAuth() {
                    const token = localStorage.getItem('token');
                    if (token) {
                        // 简化的token解析，实际应用中需要正确解析JWT
                        try {
                            const payload = JSON.parse(atob(token.split('.')[1]));
                            this.user = { email: payload.email, id: payload.sub };
                            axios.defaults.headers.common['Authorization'] = `Bearer ${token}`;
                        } catch (e) {
                            localStorage.removeItem('token');
                        }
                    }
                },

                async login() {
                    try {
                        const response = await axios.post('/api/auth/login', this.loginForm);
                        localStorage.setItem('token', response.data.token);
                        this.checkAuth();
                        this.showLogin = false;
                        this.loginForm = { email: '', password: '' };
                    } catch (error) {
                        alert('Login failed: ' + error.response.data.error);
                    }
                },

                async register() {
                    try {
                        const response = await axios.post('/api/auth/register', this.registerForm);
                        localStorage.setItem('token', response.data.token);
                        this.checkAuth();
                        this.showRegister = false;
                        this.registerForm = { email: '', displayName: '', password: '' };
                    } catch (error) {
                        alert('Registration failed: ' + error.response.data.error);
                    }
                },

                logout() {
                    localStorage.removeItem('token');
                    delete axios.defaults.headers.common['Authorization'];
                    this.user = null;
                    this.currentView = 'posts';
                },

                // 文章相关
                async loadPosts() {
                    try {
                        const response = await axios.get('/api/posts');
                        this.posts = response.data;
                    } catch (error) {
                        console.error('Failed to load posts:', error);
                    }
                },

                async loadMyPosts() {
                    if (!this.user) return;
                    try {
                        const response = await axios.get('/api/my-posts');
                        this.myPosts = response.data;
                        this.currentView = 'my-posts';
                    } catch (error) {
                        console.error('Failed to load my posts:', error);
                    }
                },

                async viewPost(post) {
                    this.selectedPost = post;
                    this.currentView = 'post';
                    await this.loadComments(post.id);
                },

                async savePost() {
                    try {
                        const postData = {
                            ...this.postForm,
                            tags: this.postForm.tags.split(',').map(t => t.trim()).filter(t => t)
                        };

                        if (this.editingPost) {
                            await axios.put(`/api/posts/${this.editingPost.id}`, postData);
                        } else {
                            await axios.post('/api/posts', postData);
                        }

                        this.closePostEditor();
                        await this.loadMyPosts();
                    } catch (error) {
                        alert('Failed to save post: ' + error.response.data.error);
                    }
                },

                editPost(post) {
                    this.editingPost = post;
                    this.postForm = {
                        title: post.title,
                        content: post.content,
                        excerpt: post.excerpt || '',
                        tags: '', // 需要从API获取标签
                        published: post.published
                    };
                    this.showPostEditor = true;
                },

                async deletePost(postId) {
                    if (confirm('Are you sure you want to delete this post?')) {
                        try {
                            await axios.delete(`/api/posts/${postId}`);
                            await this.loadMyPosts();
                        } catch (error) {
                            alert('Failed to delete post: ' + error.response.data.error);
                        }
                    }
                },

                closePostEditor() {
                    this.showPostEditor = false;
                    this.editingPost = null;
                    this.postForm = { title: '', content: '', excerpt: '', tags: '', published: false };
                },

                // 评论相关
                async loadComments(postId) {
                    try {
                        const response = await axios.get(`/api/posts/${postId}/comments`);
                        this.comments = response.data;
                    } catch (error) {
                        console.error('Failed to load comments:', error);
                    }
                },

                async addComment() {
                    if (!this.newComment.trim()) return;
                    try {
                        await axios.post(`/api/posts/${this.selectedPost.id}/comments`, {
                            content: this.newComment
                        });
                        this.newComment = '';
                        await this.loadComments(this.selectedPost.id);
                    } catch (error) {
                        alert('Failed to add comment: ' + error.response.data.error);
                    }
                },

                // 工具函数
                formatDate(dateString) {
                    return new Date(dateString).toLocaleDateString();
                }
            }
        }).mount('#app');
    </script>
</body>
</html>
```

## 评论控制器实现

**src/Controllers/Comments.hs**
```haskell
{-# LANGUAGE OverloadedStrings #-}
module Controllers.Comments where

import Web.Scotty
import Database.Persist
import Models.Blog
import Services.CommentService
import Auth.Middleware
import Control.Monad.IO.Class

-- 评论数据传输对象
data CommentDTO = CommentDTO
    { commentDtoContent :: Text
    , commentDtoParentId :: Maybe CommentId
    } deriving (Show, Generic)

instance FromJSON CommentDTO
instance ToJSON CommentDTO

-- 评论控制器
commentRoutes :: CommentService -> DbPool -> ScottyM ()
commentRoutes commentService pool = do
    -- 获取文章评论
    get "/api/posts/:postId/comments" $ do
        postIdText <- param "postId"
        case fromPathPiece postIdText of
            Nothing -> do
                status status400
                json $ object ["error" .= ("Invalid post ID" :: Text)]
            Just postId -> do
                comments <- liftIO $ getPostComments commentService postId
                json comments

    -- 添加评论（需要认证）
    post "/api/posts/:postId/comments" $ do
        claims <- requireAuth
        postIdText <- param "postId"
        commentDto <- jsonData :: ActionM CommentDTO

        case fromPathPiece postIdText of
            Nothing -> do
                status status400
                json $ object ["error" .= ("Invalid post ID" :: Text)]
            Just postId -> do
                result <- liftIO $ createComment commentService postId
                    (toSqlKey $ userId claims) commentDto
                case result of
                    Left err -> do
                        status status400
                        json $ object ["error" .= err]
                    Right comment -> do
                        status status201
                        json comment

    -- 删除评论（需要认证）
    delete "/api/comments/:id" $ do
        claims <- requireAuth
        commentIdText <- param "id"

        case fromPathPiece commentIdText of
            Nothing -> do
                status status400
                json $ object ["error" .= ("Invalid comment ID" :: Text)]
            Just commentId -> do
                result <- liftIO $ deleteComment commentService commentId
                    (toSqlKey $ userId claims)
                case result of
                    Left err -> do
                        status status400
                        json $ object ["error" .= err]
                    Right _ -> do
                        status status204
                        text ""
```

## 评论服务实现

**src/Services/CommentService.hs**
```haskell
{-# LANGUAGE OverloadedStrings #-}
module Services.CommentService where

import Database.Persist
import Database.Persist.Sql
import Data.Text (Text)
import Data.Time
import Models.Blog
import Database

-- 评论服务
data CommentService = CommentService
    { createComment :: PostId -> UserId -> CommentDTO -> IO (Either Text (Entity Comment))
    , deleteComment :: CommentId -> UserId -> IO (Either Text ())
    , getPostComments :: PostId -> IO [Entity Comment]
    , getCommentReplies :: CommentId -> IO [Entity Comment]
    }

-- 创建评论服务实例
mkCommentService :: DbPool -> CommentService
mkCommentService pool = CommentService
    { createComment = createCommentImpl pool
    , deleteComment = deleteCommentImpl pool
    , getPostComments = getPostCommentsImpl pool
    , getCommentReplies = getCommentRepliesImpl pool
    }

-- 实现函数
createCommentImpl :: DbPool -> PostId -> UserId -> CommentDTO -> IO (Either Text (Entity Comment))
createCommentImpl pool postId userId commentDto = do
    now <- getCurrentTime

    -- 验证文章是否存在
    maybePost <- runDb pool $ get postId
    case maybePost of
        Nothing -> return $ Left "Post not found"
        Just _ -> do
            let comment = Comment
                    { commentContent = commentDtoContent commentDto
                    , commentPostId = postId
                    , commentAuthorId = userId
                    , commentParentId = commentDtoParentId commentDto
                    , commentCreatedAt = now
                    , commentUpdatedAt = now
                    }

            commentId <- runDb pool $ insert comment
            return $ Right $ Entity commentId comment

deleteCommentImpl :: DbPool -> CommentId -> UserId -> IO (Either Text ())
deleteCommentImpl pool commentId userId = do
    maybeComment <- runDb pool $ get commentId
    case maybeComment of
        Nothing -> return $ Left "Comment not found"
        Just comment ->
            if commentAuthorId comment /= userId
                then return $ Left "Unauthorized"
                else do
                    -- 递归删除回复
                    replies <- runDb pool $ selectList [CommentParentId ==. Just commentId] []
                    mapM_ (\(Entity replyId _) -> runDb pool $ delete replyId) replies

                    -- 删除评论
                    runDb pool $ delete commentId
                    return $ Right ()

getPostCommentsImpl :: DbPool -> PostId -> IO [Entity Comment]
getPostCommentsImpl pool postId = runDb pool $
    selectList [CommentPostId ==. postId, CommentParentId ==. Nothing]
               [Asc CommentCreatedAt]

getCommentRepliesImpl :: DbPool -> CommentId -> IO [Entity Comment]
getCommentRepliesImpl pool commentId = runDb pool $
    selectList [CommentParentId ==. Just commentId]
               [Asc CommentCreatedAt]
```

## 主应用程序文件

**src/Main.hs**
```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Database.Persist.Sqlite
import Database.Persist.TH
import Models.Blog
import Controllers.Auth
import Controllers.Posts
import Controllers.Comments
import Services.PostService
import Services.CommentService
import Services.UserService
import Config
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.Cors

main :: IO ()
main = do
    -- 初始化数据库
    pool <- createSqlitePool "blog.db" 10
    runSqlPool (runMigration migrateAll) pool

    -- 创建服务实例
    let userService = mkUserService pool
        postService = mkPostService pool
        commentService = mkCommentService pool

    -- 启动服务器
    scotty 3000 $ do
        -- 中间件
        middleware $ cors (const $ Just simpleCorsResourcePolicy
            { corsRequestHeaders = ["content-type", "authorization"]
            , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
            })
        middleware $ staticPolicy (noDots >-> addBase "static")

        -- 路由
        authRoutes userService pool
        postRoutes postService pool
        commentRoutes commentService pool

        -- 默认路由
        get "/" $ file "static/index.html"

        -- 健康检查
        get "/health" $ json $ object ["status" .= ("ok" :: Text)]
```

## 配置文件

**src/Config.hs**
```haskell
{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.Text (Text)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)

-- 应用配置
data AppConfig = AppConfig
    { configPort :: Int
    , configDatabaseUrl :: Text
    , configJwtSecret :: Text
    , configJwtExpiry :: Int  -- 秒
    } deriving (Show)

-- 从环境变量加载配置
loadConfig :: IO AppConfig
loadConfig = do
    port <- maybe 3000 read <$> lookupEnv "PORT"
    dbUrl <- maybe "blog.db" id <$> lookupEnv "DATABASE_URL"
    jwtSecret <- maybe "your-secret-key" id <$> lookupEnv "JWT_SECRET"
    jwtExpiry <- maybe 86400 read <$> lookupEnv "JWT_EXPIRY"  -- 24小时

    return AppConfig
        { configPort = port
        , configDatabaseUrl = T.pack dbUrl
        , configJwtSecret = T.pack jwtSecret
        , configJwtExpiry = jwtExpiry
        }
```

## 项目构建配置

**package.yaml**
```yaml
name: haskell-blog
version: 0.1.0.0
github: "yourusername/haskell-blog"
license: BSD3
author: "Your Name"
maintainer: "your.email@example.com"
copyright: "2024 Your Name"

extra-source-files:
- README.md
- CHANGELOG.md

synopsis: A full-featured blog system built with Haskell
category: Web

dependencies:
- base >= 4.7 && < 5
- scotty
- persistent
- persistent-sqlite
- persistent-template
- aeson
- text
- time
- transformers
- mtl
- bytestring
- bcrypt
- jwt
- wai
- wai-middleware-static
- wai-cors
- http-types

executables:
  haskell-blog:
    main: Main.hs
    source-dirs: src
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    - -Wall
```

## 部署说明

### 本地开发
```bash
# 安装依赖
stack build

# 运行应用
stack exec haskell-blog

# 访问 http://localhost:3000
```

### 生产部署
```bash
# 构建生产版本
stack build --optimize

# 设置环境变量
export PORT=80
export DATABASE_URL="blog.db"
export JWT_SECRET="your-production-secret"

# 运行
stack exec haskell-blog
```

这个完整的博客系统包含了：
1. 用户认证和授权
2. 文章的CRUD操作
3. 评论系统
4. 标签管理
5. 搜索功能
6. 响应式前端界面
7. RESTful API设计
8. 数据库持久化
9. JWT token认证
10. CORS支持

系统采用了良好的架构分层，便于维护和扩展。
