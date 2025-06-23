# Haskell Scotty 第4节：HTTP 响应构建

## 目录
1. [响应状态码设置](#响应状态码设置)
2. [响应头操作](#响应头操作)
3. [JSON 响应处理](#json-响应处理)
4. [HTML 模板渲染](#html-模板渲染)
5. [静态文件服务](#静态文件服务)
6. [综合示例](#综合示例)

## 响应状态码设置

### 基本状态码设置

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Network.HTTP.Types.Status

main :: IO ()
main = scotty 3000 $ do
  -- 默认 200 OK
  get "/success" $ text "成功"
  
  -- 手动设置状态码
  get "/created" $ do
    status status201
    text "资源已创建"
  
  -- 404 Not Found
  get "/not-found" $ do
    status status404
    text "页面未找到"
  
  -- 500 Internal Server Error
  get "/error" $ do
    status status500
    text "服务器内部错误"
```

### 常用状态码示例

```haskell
import Network.HTTP.Types.Status

-- 成功响应
successResponses :: ScottyM ()
successResponses = do
  get "/ok" $ do
    status status200
    text "OK"
  
  get "/created" $ do
    status status201
    json $ object ["message" .= ("资源已创建" :: Text)]
  
  get "/accepted" $ do
    status status202
    text "请求已接受"
  
  get "/no-content" $ do
    status status204
    -- 204 响应不应该有内容体

-- 客户端错误响应
clientErrorResponses :: ScottyM ()
clientErrorResponses = do
  get "/bad-request" $ do
    status status400
    json $ object ["error" .= ("请求格式错误" :: Text)]
  
  get "/unauthorized" $ do
    status status401
    setHeader "WWW-Authenticate" "Bearer"
    json $ object ["error" .= ("未授权访问" :: Text)]
  
  get "/forbidden" $ do
    status status403
    json $ object ["error" .= ("禁止访问" :: Text)]
  
  get "/not-found" $ do
    status status404
    json $ object ["error" .= ("资源未找到" :: Text)]
  
  get "/method-not-allowed" $ do
    status status405
    setHeader "Allow" "GET, POST"
    json $ object ["error" .= ("方法不允许" :: Text)]

-- 服务器错误响应
serverErrorResponses :: ScottyM ()
serverErrorResponses = do
  get "/internal-error" $ do
    status status500
    json $ object ["error" .= ("内部服务器错误" :: Text)]
  
  get "/not-implemented" $ do
    status status501
    json $ object ["error" .= ("功能未实现" :: Text)]
  
  get "/service-unavailable" $ do
    status status503
    setHeader "Retry-After" "120"
    json $ object ["error" .= ("服务不可用" :: Text)]
```

## 响应头操作

### 设置响应头

```haskell
import Data.Time
import qualified Data.Text as T

-- 基本响应头设置
headerExamples :: ScottyM ()
headerExamples = do
  get "/headers" $ do
    -- 设置内容类型
    setHeader "Content-Type" "application/json; charset=utf-8"
    
    -- 设置缓存控制
    setHeader "Cache-Control" "no-cache, no-store, must-revalidate"
    setHeader "Pragma" "no-cache"
    setHeader "Expires" "0"
    
    -- 设置 CORS 头
    setHeader "Access-Control-Allow-Origin" "*"
    setHeader "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE"
    setHeader "Access-Control-Allow-Headers" "Content-Type, Authorization"
    
    json $ object ["message" .= ("带有自定义头的响应" :: Text)]

-- 动态响应头
dynamicHeaders :: ScottyM ()
dynamicHeaders = do
  get "/dynamic-headers" $ do
    currentTime <- liftIO getCurrentTime
    let timeStr = T.pack $ show currentTime
    
    -- 设置时间戳头
    setHeader "X-Timestamp" timeStr
    setHeader "X-Server" "Scotty-Haskell"
    
    -- 设置内容长度（自动计算）
    let responseBody = "当前时间: " <> timeStr
    text responseBody

-- 条件响应头
conditionalHeaders :: ScottyM ()
conditionalHeaders = do
  get "/conditional" $ do
    userAgent <- header "User-Agent"
    case userAgent of
      Just ua -> do
        setHeader "X-User-Agent-Detected" ua
        text $ "检测到用户代理: " <> ua
      Nothing -> do
        setHeader "X-User-Agent-Detected" "Unknown"
        text "未检测到用户代理"
```

### 响应头工具函数

```haskell
-- 设置安全相关头
setSecurityHeaders :: ActionM ()
setSecurityHeaders = do
  setHeader "X-Content-Type-Options" "nosniff"
  setHeader "X-Frame-Options" "DENY"
  setHeader "X-XSS-Protection" "1; mode=block"
  setHeader "Strict-Transport-Security" "max-age=31536000; includeSubDomains"

-- 设置 API 响应头
setApiHeaders :: ActionM ()
setApiHeaders = do
  setHeader "Content-Type" "application/json; charset=utf-8"
  setHeader "X-API-Version" "1.0"
  setHeader "X-RateLimit-Limit" "1000"
  setHeader "X-RateLimit-Remaining" "999"

-- 使用示例
secureApiRoute :: ScottyM ()
secureApiRoute = do
  get "/secure-api" $ do
    setSecurityHeaders
    setApiHeaders
    json $ object 
      [ "data" .= ("安全的 API 响应" :: Text)
      , "timestamp" .= ("2024-01-01T00:00:00Z" :: Text)
      ]
```

## JSON 响应处理

### 基本 JSON 响应

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import GHC.Generics

-- 定义数据类型
data User = User
  { userId :: Int
  , userName :: Text
  , userEmail :: Text
  , userActive :: Bool
  } deriving (Show, Generic)

instance ToJSON User
instance FromJSON User

data ApiResponse a = ApiResponse
  { success :: Bool
  , message :: Text
  , result :: Maybe a
  } deriving (Show, Generic)

instance ToJSON a => ToJSON (ApiResponse a)

-- JSON 响应示例
jsonResponses :: ScottyM ()
jsonResponses = do
  -- 简单 JSON 对象
  get "/json/simple" $ do
    json $ object 
      [ "name" .= ("张三" :: Text)
      , "age" .= (25 :: Int)
      , "active" .= True
      ]
  
  -- 使用自定义数据类型
  get "/json/user/:id" $ do
    userId' <- param "id"
    let user = User userId' "张三" "zhangsan@example.com" True
    json user
  
  -- 包装的 API 响应
  get "/json/wrapped" $ do
    let user = User 1 "李四" "lisi@example.com" True
    let response = ApiResponse True "用户获取成功" (Just user)
    json response
  
  -- JSON 数组
  get "/json/users" $ do
    let users = 
          [ User 1 "张三" "zhangsan@example.com" True
          , User 2 "李四" "lisi@example.com" False
          , User 3 "王五" "wangwu@example.com" True
          ]
    json users
```

### 处理 JSON 请求和响应

```haskell
-- 处理 JSON 输入
jsonInputHandling :: ScottyM ()
jsonInputHandling = do
  post "/users" $ do
    user <- jsonData :: ActionM User
    -- 这里可以进行数据库操作
    let savedUser = user { userId = 100 }  -- 模拟分配 ID
    
    status status201
    json $ ApiResponse True "用户创建成功" (Just savedUser)
  
  put "/users/:id" $ do
    userId' <- param "id"
    user <- jsonData :: ActionM User
    let updatedUser = user { userId = userId' }
    
    json $ ApiResponse True "用户更新成功" (Just updatedUser)

-- 错误处理的 JSON 响应
jsonErrorHandling :: ScottyM ()
jsonErrorHandling = do
  get "/json/error-demo" $ do
    success <- liftIO $ randomRIO (1, 10) :: ActionM Int
    if success <= 5
      then do
        status status500
        json $ ApiResponse False "随机错误发生" (Nothing :: Maybe User)
      else
        json $ ApiResponse True "操作成功" (Nothing :: Maybe User)
```

### JSON 响应格式化

```haskell
-- 美化的 JSON 响应
prettyJsonResponse :: ToJSON a => a -> ActionM ()
prettyJsonResponse value = do
  setHeader "Content-Type" "application/json; charset=utf-8"
  raw $ encodePretty value

-- 分页响应格式
data PaginatedResponse a = PaginatedResponse
  { items :: [a]
  , totalCount :: Int
  , page :: Int
  , pageSize :: Int
  , hasNext :: Bool
  , hasPrev :: Bool
  } deriving (Show, Generic)

instance ToJSON a => ToJSON (PaginatedResponse a)

paginatedJsonResponse :: ScottyM ()
paginatedJsonResponse = do
  get "/json/paginated" $ do
    page' <- param "page" `rescue` (\_ -> return 1)
    pageSize' <- param "pageSize" `rescue` (\_ -> return 10)
    
    let totalUsers = 100
    let users = take pageSize' $ drop ((page' - 1) * pageSize') 
          [ User i ("用户" <> T.pack (show i)) 
                   ("user" <> T.pack (show i) <> "@example.com") 
                   True 
          | i <- [1..totalUsers] 
          ]
    
    let response = PaginatedResponse
          { items = users
          , totalCount = totalUsers
          , page = page'
          , pageSize = pageSize'
          , hasNext = page' * pageSize' < totalUsers
          , hasPrev = page' > 1
          }
    
    json response
```

## HTML 模板渲染

### 使用 Mustache 模板

首先安装依赖：
```bash
# 在 .cabal 文件中添加
# mustache >= 2.3
```

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Text.Mustache
import qualified Data.HashMap.Strict as H

-- 基本模板渲染
templateRendering :: ScottyM ()
templateRendering = do
  get "/template/hello/:name" $ do
    name <- param "name"
    template <- liftIO $ automaticCompile ["templates"] "hello"
    case template of
      Left err -> do
        status status500
        text $ "模板错误: " <> T.pack (show err)
      Right tmpl -> do
        let context = object ["name" .= name]
        html $ renderMustache tmpl context

-- 复杂模板数据
complexTemplateData :: ScottyM ()
complexTemplateData = do
  get "/template/profile/:userId" $ do
    userId' <- param "userId"
    template <- liftIO $ automaticCompile ["templates"] "profile"
    
    case template of
      Left err -> do
        status status500
        text $ "模板错误: " <> T.pack (show err)
      Right tmpl -> do
        let user = User userId' "张三" "zhangsan@example.com" True
        let context = object
              [ "user" .= user
              , "title" .= ("用户资料 - " <> userName user :: Text)
              , "currentYear" .= (2024 :: Int)
              , "features" .= 
                  [ object ["name" .= ("邮箱验证" :: Text), "enabled" .= True]
                  , object ["name" .= ("双因素认证" :: Text), "enabled" .= False]
                  , object ["name" .= ("API 访问" :: Text), "enabled" .= True]
                  ]
              ]
        
        setHeader "Content-Type" "text/html; charset=utf-8"
        html $ renderMustache tmpl context
```

### 模板文件示例

创建 `templates/hello.mustache`：
```html
<!DOCTYPE html>
<html>
<head>
    <title>Hello {{name}}</title>
    <meta charset="utf-8">
</head>
<body>
    <h1>Hello, {{name}}!</h1>
    <p>欢迎使用 Scotty 模板系统。</p>
</body>
</html>
```

创建 `templates/profile.mustache`：
```html
<!DOCTYPE html>
<html>
<head>
    <title>{{title}}</title>
    <meta charset="utf-8">
    <style>
        body { font-family: Arial, sans-serif; margin: 40px; }
        .profile { border: 1px solid #ddd; padding: 20px; border-radius: 5px; }
        .feature { margin: 5px 0; }
        .enabled { color: green; }
        .disabled { color: red; }
    </style>
</head>
<body>
    <div class="profile">
        <h1>用户资料</h1>
        <p><strong>ID:</strong> {{user.userId}}</p>
        <p><strong>姓名:</strong> {{user.userName}}</p>
        <p><strong>邮箱:</strong> {{user.userEmail}}</p>
        <p><strong>状态:</strong> {{#user.userActive}}激活{{/user.userActive}}{{^user.userActive}}未激活{{/user.userActive}}</p>
        
        <h2>功能</h2>
        {{#features}}
        <div class="feature">
            <span>{{name}}: </span>
            <span class="{{#enabled}}enabled{{/enabled}}{{^enabled}}disabled{{/enabled}}">
                {{#enabled}}启用{{/enabled}}{{^enabled}}禁用{{/enabled}}
            </span>
        </div>
        {{/features}}
        
        <footer>
            <p><small>&copy; {{currentYear}} Scotty 应用</small></p>
        </footer>
    </div>
</body>
</html>
```

### 布局和部分模板

```haskell
-- 使用布局模板
layoutTemplateExample :: ScottyM ()
layoutTemplateExample = do
  get "/template/layout-demo" $ do
    layoutTemplate <- liftIO $ automaticCompile ["templates"] "layout"
    contentTemplate <- liftIO $ automaticCompile ["templates"] "content"
    
    case (layoutTemplate, contentTemplate) of
      (Right layout, Right content) -> do
        let contentHtml = renderMustache content $ object 
              [ "message" .= ("这是内容区域" :: Text)
              , "items" .= [1..5 :: Int]
              ]
        
        let context = object
              [ "title" .= ("布局示例" :: Text)
              , "content" .= contentHtml
              , "navigation" .= 
                  [ object ["name" .= ("首页" :: Text), "url" .= ("/" :: Text)]
                  , object ["name" .= ("关于" :: Text), "url" .= ("/about" :: Text)]
                  , object ["name" .= ("联系" :: Text), "url" .= ("/contact" :: Text)]
                  ]
              ]
        
        html $ renderMustache layout context
      _ -> do
        status status500
        text "模板加载失败"
```

## 静态文件服务

### 基本静态文件服务

```haskell
import Web.Scotty.Trans
import Network.Wai.Middleware.Static

-- 配置静态文件中间件
staticFileServer :: ScottyM ()
staticFileServer = do
  -- 服务 public 目录下的静态文件
  middleware $ staticPolicy (noDots >-> addBase "public")
  
  -- 自定义静态文件路由
  get "/static/*" $ do
    file <- param "*"
    let filePath = "public/" ++ file
    exists <- liftIO $ doesFileExist filePath
    if exists
      then do
        -- 设置适当的 Content-Type
        setContentTypeByExtension filePath
        file filePath
      else do
        status status404
        text "文件未找到"

-- 根据文件扩展名设置 Content-Type
setContentTypeByExtension :: String -> ActionM ()
setContentTypeByExtension filePath = 
  case takeExtension filePath of
    ".html" -> setHeader "Content-Type" "text/html; charset=utf-8"
    ".css"  -> setHeader "Content-Type" "text/css"
    ".js"   -> setHeader "Content-Type" "application/javascript"
    ".json" -> setHeader "Content-Type" "application/json"
    ".png"  -> setHeader "Content-Type" "image/png"
    ".jpg"  -> setHeader "Content-Type" "image/jpeg"
    ".jpeg" -> setHeader "Content-Type" "image/jpeg"
    ".gif"  -> setHeader "Content-Type" "image/gif"
    ".svg"  -> setHeader "Content-Type" "image/svg+xml"
    ".pdf"  -> setHeader "Content-Type" "application/pdf"
    _       -> setHeader "Content-Type" "application/octet-stream"
```

### 高级静态文件处理

```haskell
import System.FilePath
import System.Directory
import Data.Time
import qualified Data.ByteString.Lazy as L

-- 带缓存的静态文件服务
cachedStaticFiles :: ScottyM ()
cachedStaticFiles = do
  get "/assets/*" $ do
    file <- param "*"
    let filePath = "assets/" ++ file
    
    exists <- liftIO $ doesFileExist filePath
    if not exists
      then do
        status status404
        text "文件未找到"
      else do
        -- 获取文件信息
        modTime <- liftIO $ getModificationTime filePath
        let etag = "\"" ++ show (hash $ show modTime) ++ "\""
        
        -- 检查 If-None-Match 头
        clientEtag <- header "If-None-Match"
        if clientEtag == Just (T.pack etag)
          then do
            status status304  -- Not Modified
          else do
            -- 设置缓存头
            setHeader "ETag" (T.pack etag)
            setHeader "Cache-Control" "public, max-age=3600"
            setContentTypeByExtension filePath
            
            file filePath

-- 文件上传和服务
fileUploadAndServe :: ScottyM ()
fileUploadAndServe = do
  -- 文件上传
  post "/upload" $ do
    uploadedFile <- file "file"
    let fileName = "uploads/" ++ show (hash uploadedFile) ++ ".bin"
    
    liftIO $ L.writeFile fileName uploadedFile
    
    json $ object 
      [ "success" .= True
      , "message" .= ("文件上传成功" :: Text)
      , "fileUrl" .= ("/uploads/" ++ takeFileName fileName :: Text)
      ]
  
  -- 服务上传的文件
  get "/uploads/:filename" $ do
    filename <- param "filename"
    let filePath = "uploads/" ++ filename
    
    exists <- liftIO $ doesFileExist filePath
    if exists
      then do
        setHeader "Content-Disposition" $ 
          "attachment; filename=\"" <> T.pack filename <> "\""
        file filePath
      else do
        status status404
        json $ object ["error" .= ("文件未找到" :: Text)]
```

## 综合示例

### 完整的 Web 应用示例

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Data.Aeson
import GHC.Generics
import Network.HTTP.Types.Status
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import Text.Mustache
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- 应用数据类型
data BlogPost = BlogPost
  { postId :: Int
  , postTitle :: Text
  , postContent :: Text
  , postAuthor :: Text
  , postDate :: Text
  } deriving (Show, Generic)

instance ToJSON BlogPost
instance FromJSON BlogPost

-- 模拟数据库
blogPosts :: [BlogPost]
blogPosts = 
  [ BlogPost 1 "Haskell Web 开发" "使用 Scotty 构建 Web 应用..." "张三" "2024-01-15"
  , BlogPost 2 "函数式编程入门" "函数式编程的基本概念..." "李四" "2024-01-20"
  , BlogPost 3 "Monad 详解" "理解 Haskell 中的 Monad..." "王五" "2024-01-25"
  ]

main :: IO ()
main = scotty 3000 $ do
  -- 中间件设置
  middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase "public")
  
  -- 首页 - HTML
  get "/" $ do
    template <- liftIO $ automaticCompile ["templates"] "index"
    case template of
      Left _ -> do
        status status500
        text "模板加载失败"
      Right tmpl -> do
        let context = object 
              [ "title" .= ("我的博客" :: Text)
              , "posts" .= blogPosts
              ]
        setHeader "Content-Type" "text/html; charset=utf-8"
        html $ renderMustache tmpl context
  
  -- API 路由 - JSON
  get "/api/posts" $ do
    setApiHeaders
    json $ ApiResponse True "获取文章列表成功" (Just blogPosts)
  
  get "/api/posts/:id" $ do
    postId' <- param "id"
    setApiHeaders
    
    case find (\p -> postId p == postId') blogPosts of
      Just post -> json $ ApiResponse True "获取文章成功" (Just post)
      Nothing -> do
        status status404
        json $ ApiResponse False "文章未找到" (Nothing :: Maybe BlogPost)
  
  -- 创建文章
  post "/api/posts" $ do
    newPost <- jsonData :: ActionM BlogPost
    setApiHeaders
    
    let savedPost = newPost { postId = length blogPosts + 1 }
    status status201
    json $ ApiResponse True "文章创建成功" (Just savedPost)
  
  -- 健康检查
  get "/health" $ do
    setHeader "Content-Type" "application/json"
    json $ object 
      [ "status" .= ("healthy" :: Text)
      , "timestamp" .= ("2024-01-01T00:00:00Z" :: Text)
      , "uptime" .= (3600 :: Int)
      ]
  
  -- 错误处理示例
  get "/error-demo" $ do
    errorType <- param "type" `rescue` (\_ -> return "404")
    case errorType of
      "400" -> do
        status status400
        json $ object ["error" .= ("错误的请求参数" :: Text)]
      "401" -> do
        status status401
        setHeader "WWW-Authenticate" "Bearer"
        json $ object ["error" .= ("需要身份验证" :: Text)]
      "403" -> do
        status status403
        json $ object ["error" .= ("访问被禁止" :: Text)]
      "500" -> do
        status status500
        json $ object ["error" .= ("内部服务器错误" :: Text)]
      _ -> do
        status status404
        json $ object ["error" .= ("页面未找到" :: Text)]

-- 工具函数
setApiHeaders :: ActionM ()
setApiHeaders = do
  setHeader "Content-Type" "application/json; charset=utf-8"
  setHeader "X-API-Version" "1.0"

data ApiResponse a = ApiResponse
  { success :: Bool
  , message :: Text
  , result :: Maybe a
  } deriving (Show, Generic)

instance ToJSON a => ToJSON (ApiResponse a)
```

### 对应的模板文件

创建 `templates/index.mustache`：
```html
<!DOCTYPE html>
<html lang="zh-CN">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{{title}}</title>
    <link rel="stylesheet" href="/static/style.css">
</head>
<body>
    <header>
        <h1>{{title}}</h1>
        <nav>
            <a href="/">首页</a>
            <a href="/api/posts">API</a>
        </nav>
    </header>
    
    <main>
        <section class="posts">
            <h2>最新文章</h2>
            {{#posts}}
            <article class="post">
                <h3><a href="/api/posts/{{postId}}">{{postTitle}}</a></h3>
                <p class="meta">作者: {{postAuthor}} | 日期: {{postDate}}</p>
                <p>{{postContent}}</p>
            </article>
            {{/posts}}
            
            {{^posts}}
            <p>暂无文章</p>
            {{/posts}}
        </section>
    </main>
    
    <footer>
        <p>&copy; 2024 我的博客. All rights reserved.</p>
    </footer>
    
    <script src="/static/app.js"></script>
</body>
</html>
```

## 总结

这个教程涵盖了 Scotty 中 HTTP 响应构建的所有重要方面：

1. **状态码设置**：学会如何设置各种 HTTP 状态码
2. **响应头操作**：掌握响应头的设置和管理
3. **JSON 响应**：处理 JSON 数据的输入输出
4. **HTML 模板**：使用 Mustache 进行服务端渲染
5. **静态文件**：配置和优化静态资源服务

通过这些技术，你可以构建功能完整的 Web 应用程序，既支持 API 接口，也支持传统的 HTML 页面渲染。记住要根据具体需求选择合适的响应类型和处理方式。