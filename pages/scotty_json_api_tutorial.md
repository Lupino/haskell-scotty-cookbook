# Haskell Scotty JSON API 开发完整教程

## 第6节：JSON API 开发

### 1. Aeson 库集成

#### 1.1 依赖配置

首先在 `package.yaml` 或 `.cabal` 文件中添加必要的依赖：

```yaml
dependencies:
- base >= 4.7 && < 5
- scotty
- aeson
- aeson-pretty
- text
- bytestring
- time
- unordered-containers
- vector
- wai
- wai-cors
- http-types
```

#### 1.2 基础模块导入

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Web.Scotty
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time
import Control.Monad.IO.Class
import Network.HTTP.Types.Status
import Network.Wai.Middleware.Cors
```

### 2. JSON 序列化和反序列化

#### 2.1 定义数据模型

```haskell
-- 用户数据模型
data User = User
  { userId :: Int
  , userName :: Text
  , userEmail :: Text
  , userAge :: Int
  , userCreatedAt :: UTCTime
  } deriving (Show, Generic)

-- 用户创建请求模型
data CreateUserRequest = CreateUserRequest
  { reqUserName :: Text
  , reqUserEmail :: Text
  , reqUserAge :: Int
  } deriving (Show, Generic)

-- 用户更新请求模型
data UpdateUserRequest = UpdateUserRequest
  { updateUserName :: Maybe Text
  , updateUserEmail :: Maybe Text
  , updateUserAge :: Maybe Int
  } deriving (Show, Generic)

-- API 响应包装器
data ApiResponse a = ApiResponse
  { success :: Bool
  , message :: Text
  , apiData :: Maybe a
  } deriving (Show, Generic)

-- 错误响应模型
data ApiError = ApiError
  { errorCode :: Text
  , errorMessage :: Text
  , errorDetails :: Maybe Value
  } deriving (Show, Generic)
```

#### 2.2 JSON 实例定义

```haskell
-- User 的 JSON 实例
instance ToJSON User where
  toJSON (User uid name email age createdAt) = object
    [ "id" .= uid
    , "name" .= name
    , "email" .= email
    , "age" .= age
    , "created_at" .= createdAt
    ]

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> User
    <$> o .: "id"
    <*> o .: "name"
    <*> o .: "email"
    <*> o .: "age"
    <*> o .: "created_at"

-- CreateUserRequest 的 JSON 实例
instance FromJSON CreateUserRequest where
  parseJSON = withObject "CreateUserRequest" $ \o -> CreateUserRequest
    <$> o .: "name"
    <*> o .: "email"
    <*> o .: "age"

-- UpdateUserRequest 的 JSON 实例
instance FromJSON UpdateUserRequest where
  parseJSON = withObject "UpdateUserRequest" $ \o -> UpdateUserRequest
    <$> o .:? "name"
    <*> o .:? "email"
    <*> o .:? "age"

-- ApiResponse 的 JSON 实例
instance ToJSON a => ToJSON (ApiResponse a) where
  toJSON (ApiResponse success msg mData) = object
    [ "success" .= success
    , "message" .= msg
    , "data" .= mData
    ]

-- ApiError 的 JSON 实例
instance ToJSON ApiError where
  toJSON (ApiError code msg details) = object
    [ "error_code" .= code
    , "error_message" .= msg
    , "error_details" .= details
    ]
```

#### 2.3 自定义 JSON 选项

```haskell
-- 自定义 JSON 选项，用于字段名转换
userOptions :: Options
userOptions = defaultOptions
  { fieldLabelModifier = \case
      "userId" -> "id"
      "userName" -> "name"
      "userEmail" -> "email"
      "userAge" -> "age"
      "userCreatedAt" -> "created_at"
      other -> other
  }

-- 使用自定义选项的实例
instance ToJSON User where
  toJSON = genericToJSON userOptions

instance FromJSON User where
  parseJSON = genericParseJSON userOptions
```

### 3. RESTful API 设计原则

#### 3.1 资源路由设计

```haskell
-- 模拟数据库
type UserDB = [User]

-- 初始化用户数据
initialUsers :: IO (IORef UserDB)
initialUsers = do
  now <- getCurrentTime
  newIORef 
    [ User 1 "Alice" "alice@example.com" 25 now
    , User 2 "Bob" "bob@example.com" 30 now
    ]

-- RESTful 路由定义
setupRoutes :: IORef UserDB -> ScottyM ()
setupRoutes userDB = do
  -- 获取所有用户 GET /api/v1/users
  get "/api/v1/users" $ do
    users <- liftIO $ readIORef userDB
    json $ ApiResponse True "Users retrieved successfully" (Just users)
  
  -- 获取单个用户 GET /api/v1/users/:id
  get "/api/v1/users/:id" $ do
    uid <- param "id"
    users <- liftIO $ readIORef userDB
    case findUser uid users of
      Just user -> json $ ApiResponse True "User found" (Just user)
      Nothing -> do
        status status404
        json $ ApiError "USER_NOT_FOUND" "User not found" Nothing
  
  -- 创建用户 POST /api/v1/users
  post "/api/v1/users" $ do
    req <- jsonData :: ActionM CreateUserRequest
    users <- liftIO $ readIORef userDB
    now <- liftIO getCurrentTime
    let newId = getNextId users
        newUser = User newId (reqUserName req) (reqUserEmail req) (reqUserAge req) now
    liftIO $ modifyIORef userDB (newUser:)
    status status201
    json $ ApiResponse True "User created successfully" (Just newUser)
  
  -- 更新用户 PUT /api/v1/users/:id
  put "/api/v1/users/:id" $ do
    uid <- param "id"
    req <- jsonData :: ActionM UpdateUserRequest
    users <- liftIO $ readIORef userDB
    case updateUser uid req users of
      Just updatedUsers -> do
        liftIO $ writeIORef userDB updatedUsers
        let updatedUser = findUser uid updatedUsers
        json $ ApiResponse True "User updated successfully" updatedUser
      Nothing -> do
        status status404
        json $ ApiError "USER_NOT_FOUND" "User not found" Nothing
  
  -- 删除用户 DELETE /api/v1/users/:id
  delete "/api/v1/users/:id" $ do
    uid <- param "id"
    users <- liftIO $ readIORef userDB
    if any (\u -> userId u == uid) users
      then do
        let filteredUsers = filter (\u -> userId u /= uid) users
        liftIO $ writeIORef userDB filteredUsers
        json $ ApiResponse True "User deleted successfully" (Nothing :: Maybe User)
      else do
        status status404
        json $ ApiError "USER_NOT_FOUND" "User not found" Nothing
```

#### 3.2 辅助函数

```haskell
import Data.IORef

-- 查找用户
findUser :: Int -> [User] -> Maybe User
findUser uid = find (\u -> userId u == uid)

-- 获取下一个 ID
getNextId :: [User] -> Int
getNextId [] = 1
getNextId users = maximum (map userId users) + 1

-- 更新用户
updateUser :: Int -> UpdateUserRequest -> [User] -> Maybe [User]
updateUser uid req users = do
  user <- findUser uid users
  let updatedUser = user
        { userName = maybe (userName user) id (updateUserName req)
        , userEmail = maybe (userEmail user) id (updateUserEmail req)
        , userAge = maybe (userAge user) id (updateUserAge req)
        }
  return $ map (\u -> if userId u == uid then updatedUser else u) users
```

### 4. API 版本管理

#### 4.1 版本路由策略

```haskell
-- 版本 1 API
setupV1Routes :: IORef UserDB -> ScottyM ()
setupV1Routes userDB = do
  get "/api/v1/users" $ do
    users <- liftIO $ readIORef userDB
    json users  -- 简单返回用户列表
  
  get "/api/v1/users/:id" $ do
    uid <- param "id"
    users <- liftIO $ readIORef userDB
    case findUser uid users of
      Just user -> json user
      Nothing -> do
        status status404
        json $ object ["error" .= ("User not found" :: Text)]

-- 版本 2 API（增强版本）
setupV2Routes :: IORef UserDB -> ScottyM ()
setupV2Routes userDB = do
  get "/api/v2/users" $ do
    users <- liftIO $ readIORef userDB
    json $ ApiResponse True "Users retrieved successfully" (Just users)
  
  get "/api/v2/users/:id" $ do
    uid <- param "id"
    users <- liftIO $ readIORef userDB
    case findUser uid users of
      Just user -> json $ ApiResponse True "User found" (Just user)
      Nothing -> do
        status status404
        json $ ApiError "USER_NOT_FOUND" "User not found" Nothing

-- 版本内容协商
setupContentNegotiation :: IORef UserDB -> ScottyM ()
setupContentNegotiation userDB = do
  get "/api/users" $ do
    version <- header "API-Version" `rescue` (\_ -> return (Just "v1"))
    case version of
      Just "v2" -> do
        users <- liftIO $ readIORef userDB
        json $ ApiResponse True "Users retrieved successfully" (Just users)
      _ -> do
        users <- liftIO $ readIORef userDB
        json users
```

#### 4.2 版本兼容性处理

```haskell
-- 版本兼容的数据模型
data UserV1 = UserV1
  { userV1Id :: Int
  , userV1Name :: Text
  , userV1Email :: Text
  } deriving (Show, Generic)

data UserV2 = UserV2
  { userV2Id :: Int
  , userV2Name :: Text
  , userV2Email :: Text
  , userV2Age :: Int
  , userV2CreatedAt :: UTCTime
  } deriving (Show, Generic)

instance ToJSON UserV1
instance ToJSON UserV2

-- 版本转换函数
toUserV1 :: User -> UserV1
toUserV1 (User uid name email _ _) = UserV1 uid name email

toUserV2 :: User -> UserV2
toUserV2 (User uid name email age createdAt) = UserV2 uid name email age createdAt
```

### 5. 错误处理和状态码

#### 5.1 统一错误处理

```haskell
-- 错误类型定义
data AppError 
  = ValidationError Text
  | NotFoundError Text
  | DatabaseError Text
  | AuthorizationError Text
  | InternalError Text
  deriving (Show)

-- 错误到 HTTP 状态码的映射
errorToStatus :: AppError -> Status
errorToStatus (ValidationError _) = status400
errorToStatus (NotFoundError _) = status404
errorToStatus (DatabaseError _) = status500
errorToStatus (AuthorizationError _) = status403
errorToStatus (InternalError _) = status500

-- 错误到 JSON 的转换
errorToJson :: AppError -> ApiError
errorToJson (ValidationError msg) = ApiError "VALIDATION_ERROR" msg Nothing
errorToJson (NotFoundError msg) = ApiError "NOT_FOUND" msg Nothing
errorToJson (DatabaseError msg) = ApiError "DATABASE_ERROR" msg Nothing
errorToJson (AuthorizationError msg) = ApiError "AUTHORIZATION_ERROR" msg Nothing
errorToJson (InternalError msg) = ApiError "INTERNAL_ERROR" msg Nothing

-- 错误处理中间件
handleError :: AppError -> ActionM ()
handleError err = do
  status (errorToStatus err)
  json (errorToJson err)
```

#### 5.2 验证和错误处理

```haskell
-- 验证函数
validateCreateUserRequest :: CreateUserRequest -> Either AppError CreateUserRequest
validateCreateUserRequest req
  | T.null (reqUserName req) = Left (ValidationError "Name cannot be empty")
  | T.null (reqUserEmail req) = Left (ValidationError "Email cannot be empty")
  | not (T.isInfixOf "@" (reqUserEmail req)) = Left (ValidationError "Invalid email format")
  | reqUserAge req < 0 = Left (ValidationError "Age must be positive")
  | reqUserAge req > 150 = Left (ValidationError "Age must be realistic")
  | otherwise = Right req

-- 带验证的路由
post "/api/v1/users" $ do
  req <- jsonData :: ActionM CreateUserRequest
  case validateCreateUserRequest req of
    Left err -> handleError err
    Right validReq -> do
      users <- liftIO $ readIORef userDB
      now <- liftIO getCurrentTime
      let newId = getNextId users
          newUser = User newId (reqUserName validReq) (reqUserEmail validReq) (reqUserAge validReq) now
      liftIO $ modifyIORef userDB (newUser:)
      status status201
      json $ ApiResponse True "User created successfully" (Just newUser)
```

#### 5.3 全局异常处理

```haskell
-- 全局异常处理中间件
setupGlobalErrorHandling :: ScottyM ()
setupGlobalErrorHandling = do
  -- JSON 解析错误处理
  defaultHandler $ \str -> do
    status status400
    json $ ApiError "BAD_REQUEST" "Invalid JSON format" (Just (String (TL.toStrict str)))
  
  -- 404 处理
  notFound $ do
    status status404
    json $ ApiError "NOT_FOUND" "Endpoint not found" Nothing

-- 主应用程序
main :: IO ()
main = do
  userDB <- initialUsers
  scotty 3000 $ do
    -- CORS 中间件
    middleware $ cors (const $ Just corsPolicy)
    
    -- 全局错误处理
    setupGlobalErrorHandling
    
    -- API 路由
    setupV1Routes userDB
    setupV2Routes userDB
    
    -- 健康检查端点
    get "/health" $ do
      json $ object 
        [ "status" .= ("healthy" :: Text)
        , "timestamp" .= ("2024-01-01T00:00:00Z" :: Text)
        ]

-- CORS 策略
corsPolicy :: CorsResourcePolicy
corsPolicy = CorsResourcePolicy
  { corsOrigins = Nothing
  , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
  , corsRequestHeaders = ["Content-Type", "Authorization", "API-Version"]
  , corsExposedHeaders = Nothing
  , corsMaxAge = Nothing
  , corsVaryOrigin = False
  , corsRequireOrigin = False
  , corsIgnoreFailures = False
  }
```

### 6. 完整示例应用

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Web.Scotty
import Data.Aeson
import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Control.Monad.IO.Class
import Network.HTTP.Types.Status
import Network.Wai.Middleware.Cors
import Data.IORef
import Data.List (find)

-- [所有之前定义的数据类型和函数]

main :: IO ()
main = do
  putStrLn "Starting JSON API server on port 3000..."
  userDB <- initialUsers
  scotty 3000 $ do
    middleware $ cors (const $ Just corsPolicy)
    setupGlobalErrorHandling
    setupV1Routes userDB
    setupV2Routes userDB
    
    get "/" $ do
      json $ object 
        [ "message" .= ("Welcome to Haskell Scotty JSON API" :: Text)
        , "version" .= ("1.0.0" :: Text)
        , "endpoints" .= 
          [ "/api/v1/users" :: Text
          , "/api/v2/users"
          , "/health"
          ]
        ]
```

### 7. 测试 API

使用 curl 测试 API：

```bash
# 获取所有用户
curl -X GET http://localhost:3000/api/v1/users

# 获取单个用户
curl -X GET http://localhost:3000/api/v1/users/1

# 创建用户
curl -X POST http://localhost:3000/api/v1/users \
  -H "Content-Type: application/json" \
  -d '{"name":"Charlie","email":"charlie@example.com","age":28}'

# 更新用户
curl -X PUT http://localhost:3000/api/v1/users/1 \
  -H "Content-Type: application/json" \
  -d '{"name":"Alice Updated","age":26}'

# 删除用户
curl -X DELETE http://localhost:3000/api/v1/users/1

# 版本 2 API
curl -X GET http://localhost:3000/api/v2/users

# 健康检查
curl -X GET http://localhost:3000/health
```

这个教程涵盖了 Haskell Scotty 中 JSON API 开发的所有核心概念，包括 Aeson 集成、序列化/反序列化、RESTful 设计、版本管理和错误处理。通过这些示例，您可以构建健壮的、可维护的 JSON API 服务。