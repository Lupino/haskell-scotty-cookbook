# Haskell Scotty HTTP 请求处理

## 基础设置

### 依赖配置 (stack.yaml / cabal)
```yaml
# stack.yaml
dependencies:
- scotty
- wai
- wai-extra
- text
- bytestring
- aeson
- wai-middleware-static
- wai-cors
```

### 基本服务器设置
```haskell
{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Aeson (Value, object, (.=))

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    text "Hello Scotty!"
  
  get "/health" $ do
    json $ object ["status" .= ("ok" :: Text)]
```

## 1. 获取请求参数

### URL 查询参数 (Query Parameters)
```haskell
import Web.Scotty
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Control.Monad.IO.Class (liftIO)

-- 获取单个查询参数
get "/users" $ do
  -- 必需参数
  userId <- param "id" :: ActionM Text
  text $ "User ID: " <> userId

-- 可选参数
get "/posts" $ do
  -- 可选参数，带默认值
  page <- param "page" `rescue` (\_ -> return "1")
  limit <- param "limit" `rescue` (\_ -> return "10")
  
  json $ object [
    "page" .= page,
    "limit" .= limit
  ]

-- 获取所有查询参数
get "/search" $ do
  allParams <- params
  liftIO $ print allParams  -- 打印所有参数
  json $ object ["params" .= show allParams]

-- 安全的参数获取
get "/safe-params" $ do
  mUserId <- rescue (Just <$> param "user_id") (\_ -> return Nothing)
  case mUserId of
    Just uid -> text $ "User ID: " <> uid
    Nothing -> do
      status status400
      text "Missing user_id parameter"
```

### 路径参数 (Route Parameters)
```haskell
-- 基本路径参数
get "/users/:id" $ do
  userId <- param "id" :: ActionM Text
  text $ "User ID from path: " <> userId

-- 多个路径参数
get "/users/:userId/posts/:postId" $ do
  userId <- param "userId" :: ActionM Text
  postId <- param "postId" :: ActionM Text
  
  json $ object [
    "userId" .= userId,
    "postId" .= postId
  ]

-- 数字类型参数
get "/products/:id" $ do
  productId <- param "id" :: ActionM Int
  json $ object ["productId" .= productId]
  
-- 处理参数类型错误
get "/safe-numeric/:id" $ do
  result <- rescue (Right <$> (param "id" :: ActionM Int)) 
                   (\_ -> return $ Left "Invalid ID format")
  case result of
    Right pid -> json $ object ["productId" .= pid]
    Left err -> do
      status status400
      text $ T.pack err
```

## 2. 处理表单数据和 URL 编码

### application/x-www-form-urlencoded
```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.Wai (requestBody)

-- 处理表单提交
post "/login" $ do
  username <- param "username" :: ActionM Text
  password <- param "password" :: ActionM Text
  
  -- 简单验证
  if username == "admin" && password == "secret"
    then json $ object ["status" .= ("success" :: Text)]
    else do
      status status401
      json $ object ["error" .= ("Invalid credentials" :: Text)]

-- 处理复杂表单
post "/user-profile" $ do
  name <- param "name" :: ActionM Text
  email <- param "email" :: ActionM Text
  age <- param "age" :: ActionM Int
  
  -- 验证邮箱格式
  if "@" `T.isInfixOf` email
    then json $ object [
      "message" .= ("Profile updated" :: Text),
      "user" .= object [
        "name" .= name,
        "email" .= email,
        "age" .= age
      ]
    ]
    else do
      status status400
      json $ object ["error" .= ("Invalid email format" :: Text)]

-- 获取所有表单数据
post "/form-data" $ do
  allFormData <- params
  json $ object ["formData" .= show allFormData]
```

### JSON 数据处理
```haskell
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import GHC.Generics

-- 定义数据类型
data User = User {
  userName :: Text,
  userEmail :: Text,
  userAge :: Int
} deriving (Generic, Show)

instance FromJSON User
instance ToJSON User

-- 处理 JSON 请求
post "/api/users" $ do
  user <- jsonData :: ActionM User
  
  -- 处理用户数据
  liftIO $ putStrLn $ "Received user: " ++ show user
  
  -- 返回响应
  json $ object [
    "message" .= ("User created" :: Text),
    "user" .= user
  ]

-- 处理 JSON 解析错误
post "/api/safe-json" $ do
  result <- rescue (Right <$> jsonData) (\_ -> return $ Left "Invalid JSON")
  case result of
    Right (user :: User) -> json $ object ["user" .= user]
    Left err -> do
      status status400
      json $ object ["error" .= err]
```

## 3. 请求头信息获取

### 基本请求头处理
```haskell
import Network.Wai (requestHeaders)
import Network.HTTP.Types (hAuthorization, hUserAgent, hContentType)
import Data.ByteString.Char8 as BS

-- 获取请求头
get "/headers" $ do
  req <- request
  let headers = requestHeaders req
  
  -- 查找特定请求头
  let userAgent = lookup hUserAgent headers
  let contentType = lookup hContentType headers
  
  json $ object [
    "userAgent" .= (fmap (T.pack . BS.unpack) userAgent),
    "contentType" .= (fmap (T.pack . BS.unpack) contentType),
    "allHeaders" .= (map (\(k, v) -> T.pack (BS.unpack k) <> ": " <> T.pack (BS.unpack v)) headers)
  ]

-- 获取认证信息
get "/protected" $ do
  req <- request
  let authHeader = lookup hAuthorization (requestHeaders req)
  
  case authHeader of
    Just auth -> do
      let authStr = T.pack $ BS.unpack auth
      if "Bearer " `T.isPrefixOf` authStr
        then do
          let token = T.drop 7 authStr
          json $ object ["message" .= ("Authenticated" :: Text), "token" .= token]
        else do
          status status401
          json $ object ["error" .= ("Invalid authorization format" :: Text)]
    Nothing -> do
      status status401
      json $ object ["error" .= ("Authorization header required" :: Text)]

-- 自定义请求头处理
get "/api/data" $ do
  req <- request
  let headers = requestHeaders req
  
  -- 查找自定义头
  let apiKey = lookup "X-API-Key" headers
  let clientVersion = lookup "X-Client-Version" headers
  
  case apiKey of
    Just key -> 
      if BS.unpack key == "secret-api-key"
        then json $ object ["data" .= ("sensitive data" :: Text)]
        else do
          status status403
          json $ object ["error" .= ("Invalid API key" :: Text)]
    Nothing -> do
      status status401
      json $ object ["error" .= ("API key required" :: Text)]
```

## 4. 请求体处理

### 原始请求体处理
```haskell
import qualified Data.ByteString.Lazy as L

-- 获取原始请求体
post "/raw-body" $ do
  bodyContent <- body
  let bodyText = decodeUtf8 bodyContent
  
  json $ object [
    "bodySize" .= L.length bodyContent,
    "bodyContent" .= bodyText
  ]

-- 处理文本数据
post "/text-data" $ do
  req <- request
  let contentType = lookup hContentType (requestHeaders req)
  
  case contentType of
    Just ct | BS.isPrefixOf "text/plain" ct -> do
      bodyContent <- body
      let textContent = decodeUtf8 bodyContent
      json $ object ["text" .= textContent]
    _ -> do
      status status400
      json $ object ["error" .= ("Content-Type must be text/plain" :: Text)]

-- 条件处理不同内容类型
post "/multi-content" $ do
  req <- request
  let contentType = lookup hContentType (requestHeaders req)
  
  case contentType of
    Just ct 
      | BS.isPrefixOf "application/json" ct -> do
          user <- jsonData :: ActionM User
          json $ object ["type" .= ("json" :: Text), "data" .= user]
      | BS.isPrefixOf "application/x-www-form-urlencoded" ct -> do
          name <- param "name" :: ActionM Text
          json $ object ["type" .= ("form" :: Text), "name" .= name]
      | otherwise -> do
          status status415
          json $ object ["error" .= ("Unsupported media type" :: Text)]
    Nothing -> do
      status status400
      json $ object ["error" .= ("Content-Type header required" :: Text)]
```

## 5. 文件上传处理

### 基本文件上传
```haskell
import Network.Wai.Parse (parseRequestBody, lbsBackEnd)
import qualified Data.ByteString.Lazy as L
import System.IO

-- 处理文件上传 (需要 wai-extra)
post "/upload" $ do
  req <- request  
  (params, files) <- liftIO $ parseRequestBody lbsBackEnd req
  
  case files of
    [] -> do
      status status400
      json $ object ["error" .= ("No file uploaded" :: Text)]
    (fieldName, fileInfo):_ -> do
      let fileName = fileInfoFileName fileInfo
          fileContent = fileInfoContent fileInfo
          fileSize = L.length fileContent
      
      -- 保存文件
      liftIO $ L.writeFile ("uploads/" ++ T.unpack (T.fromStrict fileName)) fileContent
      
      json $ object [
        "message" .= ("File uploaded successfully" :: Text),
        "fileName" .= T.fromStrict fileName,
        "fileSize" .= fileSize
      ]

-- 多文件上传处理
post "/upload-multiple" $ do
  req <- request
  (params, files) <- liftIO $ parseRequestBody lbsBackEnd req
  
  let processFile (fieldName, fileInfo) = do
        let fileName = fileInfoFileName fileInfo
            fileContent = fileInfoContent fileInfo
        liftIO $ L.writeFile ("uploads/" ++ T.unpack (T.fromStrict fileName)) fileContent
        return $ object [
          "fileName" .= T.fromStrict fileName,
          "size" .= L.length fileContent
        ]
  
  uploadedFiles <- mapM processFile files
  
  json $ object [
    "message" .= ("Files uploaded successfully" :: Text),
    "files" .= uploadedFiles
  ]
```

### 文件上传验证
```haskell
import Data.ByteString as BS
import System.FilePath (takeExtension)

-- 文件类型验证
validateFileType :: Text -> Bool
validateFileType fileName = 
  let ext = T.toLower $ T.pack $ takeExtension $ T.unpack fileName
  in ext `elem` [".jpg", ".jpeg", ".png", ".gif", ".pdf"]

-- 带验证的文件上传
post "/upload-validated" $ do
  req <- request
  (params, files) <- liftIO $ parseRequestBody lbsBackEnd req
  
  case files of
    [] -> do
      status status400
      json $ object ["error" .= ("No file uploaded" :: Text)]
    (fieldName, fileInfo):_ -> do
      let fileName = T.fromStrict $ fileInfoFileName fileInfo
          fileContent = fileInfoContent fileInfo
          fileSize = L.length fileContent
      
      -- 验证文件类型
      if not (validateFileType fileName)
        then do
          status status400
          json $ object ["error" .= ("Invalid file type" :: Text)]
        -- 验证文件大小 (5MB 限制)
        else if fileSize > 5 * 1024 * 1024
          then do
            status status413
            json $ object ["error" .= ("File too large" :: Text)]
          else do
            -- 安全文件名处理
            let safeFileName = T.filter (\c -> c /= '/' && c /= '\\') fileName
            liftIO $ L.writeFile ("uploads/" ++ T.unpack safeFileName) fileContent
            
            json $ object [
              "message" .= ("File uploaded successfully" :: Text),
              "fileName" .= safeFileName,
              "fileSize" .= fileSize
            ]
```

## 中间件和高级功能

### 自定义中间件
```haskell
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static

-- CORS 中间件
main :: IO ()
main = do
  let corsPolicy = simpleCorsResourcePolicy {
    corsRequestHeaders = ["Content-Type", "Authorization"],
    corsMethods = ["GET", "POST", "PUT", "DELETE"]
  }
  
  scotty 3000 $ do
    middleware $ cors (const $ Just corsPolicy)
    middleware logStdoutDev  -- 日志中间件
    middleware $ staticPolicy (noDots >-> addBase "static")  -- 静态文件
    
    -- 认证中间件
    middleware $ \app req respond -> do
      let path = rawPathInfo req
      if "/api/" `BS.isPrefixOf` path
        then case lookup hAuthorization (requestHeaders req) of
          Just _ -> app req respond
          Nothing -> respond $ responseLBS status401 [] "Unauthorized"
        else app req respond
    
    -- 路由定义
    routes

-- 路由定义
routes :: ScottyM ()
routes = do
  get "/" $ text "Welcome!"
  
  get "/api/protected" $ do
    json $ object ["message" .= ("You are authenticated!" :: Text)]
```

### 错误处理
```haskell
-- 全局错误处理
import Control.Exception (try, SomeException)

-- 自定义错误处理
handleErrors :: ActionM () -> ActionM ()
handleErrors action = do
  result <- liftIO $ try $ runActionToIO action
  case result of
    Left (e :: SomeException) -> do
      status status500
      json $ object ["error" .= show e]
    Right _ -> return ()

-- 使用示例
get "/error-prone" $ handleErrors $ do
  -- 可能出错的操作
  result <- liftIO $ readFile "nonexistent.txt"
  text $ T.pack result

-- 参数验证中间件
validateRequired :: [Text] -> ActionM () -> ActionM ()
validateRequired requiredParams action = do
  allParams <- params
  let paramNames = map fst allParams
      missing = filter (`notElem` paramNames) requiredParams
  
  if null missing
    then action
    else do
      status status400
      json $ object [
        "error" .= ("Missing required parameters" :: Text),
        "missing" .= missing
      ]

-- 使用验证中间件
post "/validated-endpoint" $ validateRequired ["name", "email"] $ do
  name <- param "name" :: ActionM Text
  email <- param "email" :: ActionM Text
  json $ object ["message" .= ("Data valid" :: Text)]
```

## 实用工具函数

### 响应辅助函数
```haskell
-- 标准化 JSON 响应
jsonSuccess :: ToJSON a => Text -> a -> ActionM ()
jsonSuccess message dat = json $ object [
  "success" .= True,
  "message" .= message,
  "data" .= dat
]

jsonError :: Text -> ActionM ()
jsonError message = do
  status status400
  json $ object [
    "success" .= False,
    "error" .= message
  ]

-- 分页辅助
handlePagination :: ActionM (Int, Int)  -- (offset, limit)
handlePagination = do
  page <- param "page" `rescue` (\_ -> return "1") :: ActionM Text
  limit <- param "limit" `rescue` (\_ -> return "10") :: ActionM Text
  
  let pageNum = max 1 (read $ T.unpack page)
      limitNum = min 100 (max 1 (read $ T.unpack limit))  -- 最大 100
      offset = (pageNum - 1) * limitNum
  
  return (offset, limitNum)

-- 使用示例
get "/users" $ do
  (offset, limit) <- handlePagination
  -- 查询数据库逻辑...
  json $ object [
    "users" .= ([] :: [Value]),  -- 实际用户数据
    "pagination" .= object [
      "offset" .= offset,
      "limit" .= limit
    ]
  ]
```

这份指南展示了如何在 Haskell 的 Scotty 框架中处理各种 HTTP 请求，包括参数获取、请求体处理、文件上传等功能。Scotty 提供了简洁的 DSL 来构建 Web 应用，同时保持了 Haskell 的类型安全特性。