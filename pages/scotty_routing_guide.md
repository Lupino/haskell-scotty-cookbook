# 第2节：路由的艺术 - 构建清晰的 URL 体系

## HTTP 协议基础回顾

**HTTP 方法**：
- `GET` - 获取资源，幂等且安全
- `POST` - 创建资源或提交数据
- `PUT` - 更新整个资源
- `PATCH` - 部分更新资源
- `DELETE` - 删除资源

**HTTP 状态码**：
- `2xx` - 成功 (200 OK, 201 Created, 204 No Content)
- `3xx` - 重定向 (301 Moved, 302 Found)
- `4xx` - 客户端错误 (400 Bad Request, 404 Not Found)
- `5xx` - 服务器错误 (500 Internal Error, 503 Service Unavailable)

## Scotty 路由定义语法

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Data.Text.Lazy (Text)

main :: IO ()
main = scotty 3000 $ do
    -- 基础路由
    get "/" $ text "欢迎来到我的 API!"
    
    -- RESTful 路由
    get "/users" $ json [("message" :: Text, "获取所有用户")]
    post "/users" $ json [("message" :: Text, "创建新用户")]
    put "/users/:id" $ do
        userId <- param "id"
        json [("message" :: Text, "更新用户 " <> userId)]
    delete "/users/:id" $ do
        userId <- param "id"
        json [("message" :: Text, "删除用户 " <> userId)]
    
    -- 静态路由
    get "/about" $ html "<h1>关于页面</h1>"
    
    -- 多级路径
    get "/api/v1/health" $ json [("status" :: Text, "healthy")]
    
    -- 404 处理
    notFound $ do
        status status404
        json [("error" :: Text, "页面未找到")]
```

## 路径参数和查询参数

```haskell
-- 路径参数
get "/users/:id" $ do
    userId <- param "id"
    json [("userId" :: Text, userId)]

-- 多个路径参数
get "/users/:userId/posts/:postId" $ do
    userId <- param "userId"
    postId <- param "postId"
    json [("userId" :: Text, userId), ("postId" :: Text, postId)]

-- 查询参数 (带默认值)
get "/search" $ do
    query <- param "q"  -- 必需参数
    page <- param "page" `rescue` (\_ -> return "1")  -- 可选参数
    limit <- param "limit" `rescue` (\_ -> return "10")
    json [("query" :: Text, query), ("page" :: Text, page)]

-- 混合参数
get "/categories/:category/products" $ do
    category <- param "category"  -- 路径参数
    minPrice <- param "min_price" `rescue` (\_ -> return "0")  -- 查询参数
    sortBy <- param "sort" `rescue` (\_ -> return "name")
    json [("category" :: Text, category), ("sort" :: Text, sortBy)]

-- 参数验证
get "/users/:id/validate" $ do
    userIdStr <- param "id"
    case readMaybe (T.unpack userIdStr) of
        Just userId -> 
            if userId > 0 
            then json [("valid" :: Text, "true"), ("userId" :: Int, userId)]
            else do
                status status400
                json [("error" :: Text, "用户ID必须大于0")]
        Nothing -> do
            status status400
            json [("error" :: Text, "无效的用户ID格式")]

-- 通配符参数
get "/files/*" $ do
    filepath <- param "0"  -- 通配符使用数字索引
    json [("filepath" :: Text, filepath)]

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing
```

## 路由匹配规则和优先级

```haskell
-- 1. 精确匹配优先级最高
get "/users/admin" $ json [("message" :: Text, "管理员面板")]

-- 2. 参数匹配（除了 "admin" 外的所有路径）
get "/users/:id" $ do
    userId <- param "id"
    json [("userId" :: Text, userId)]

-- 3. 静态路径优先于参数路径
get "/api/users/stats" $ json [("message" :: Text, "用户统计")]
get "/api/:resource/stats" $ do
    resource <- param "resource"
    json [("resource" :: Text, resource)]

-- 4. 路由定义顺序很重要 - 具体路由在前
get "/products/featured" $ json [("message" :: Text, "特色产品")]
get "/products/categories" $ json [("message" :: Text, "产品分类")]
get "/products/:id" $ do  -- 通用路由放在后面
    productId <- param "id"
    json [("productId" :: Text, productId)]

-- 5. 通配符匹配优先级最低
get "/files/*" $ do
    filepath <- param "0"
    json [("path" :: Text, filepath)]

-- 6. 多段通配符
get "/docs/*/download/*" $ do
    section <- param "0"
    filename <- param "1"
    json [("section" :: Text, section), ("filename" :: Text, filename)]

-- 7. 条件路由（基于请求头）
get "/content" $ do
    acceptHeader <- header "Accept" `rescue` (\_ -> return Nothing)
    case acceptHeader of
        Just "application/json" -> json [("type" :: Text, "json")]
        Just "text/html" -> html "<h1>HTML Content</h1>"
        _ -> text "Plain text"

-- 8. 认证中间件示例
get "/protected/:resource" $ do
    authHeader <- header "Authorization" `rescue` (\_ -> return Nothing)
    case authHeader of
        Just token -> do
            resource <- param "resource"
            json [("resource" :: Text, resource)]
        Nothing -> do
            status status401
            json [("error" :: Text, "需要认证")]
```

## 路由组织和模块化

### 主应用文件 (Main.hs)
```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import qualified UserRoutes
import qualified ProductRoutes
import qualified AdminRoutes

main :: IO ()
main = scotty 3000 $ do
    -- 根路由
    get "/" $ json [("message" :: Text, "API v1.0.0")]
    get "/health" $ json [("status" :: Text, "healthy")]
    
    -- 加载模块化路由
    UserRoutes.routes
    ProductRoutes.routes
    AdminRoutes.routes
    
    -- 全局错误处理
    notFound $ do
        status status404
        json [("error" :: Text, "API 端点未找到")]
```

### 用户路由模块 (UserRoutes.hs)
```haskell
{-# LANGUAGE OverloadedStrings #-}

module UserRoutes (routes) where

import Web.Scotty
import Data.Text.Lazy (Text)

routes :: ScottyM ()
routes = do
    get "/users" $ do
        page <- param "page" `rescue` (\_ -> return "1")
        limit <- param "limit" `rescue` (\_ -> return "10")
        json [("message" :: Text, "用户列表"), ("page" :: Text, page)]
    
    get "/users/:id" $ do
        userId <- param "id"
        json [("userId" :: Text, userId)]
    
    post "/users" $ json [("message" :: Text, "创建用户")]
    put "/users/:id" $ do
        userId <- param "id"
        json [("message" :: Text, "更新用户"), ("userId" :: Text, userId)]
    delete "/users/:id" $ do
        userId <- param "id"
        json [("message" :: Text, "删除用户"), ("userId" :: Text, userId)]
    
    -- 嵌套资源
    get "/users/:id/posts" $ do
        userId <- param "id"
        json [("userId" :: Text, userId), ("type" :: Text, "posts")]
    
    get "/users/:id/followers" $ do
        userId <- param "id"
        json [("userId" :: Text, userId), ("type" :: Text, "followers")]
```

### 产品路由模块 (ProductRoutes.hs)
```haskell
{-# LANGUAGE OverloadedStrings #-}

module ProductRoutes (routes) where

import Web.Scotty
import Data.Text.Lazy (Text)

routes :: ScottyM ()
routes = do
    get "/products" $ do
        category <- param "category" `rescue` (\_ -> return "all")
        sortBy <- param "sort" `rescue` (\_ -> return "name")
        json [("category" :: Text, category), ("sort" :: Text, sortBy)]
    
    get "/products/:id" $ do
        productId <- param "id"
        json [("productId" :: Text, productId)]
    
    get "/categories" $ json [("message" :: Text, "产品分类")]
    
    get "/categories/:id/products" $ do
        categoryId <- param "id"
        json [("categoryId" :: Text, categoryId)]
    
    get "/search/products" $ do
        query <- param "q"
        json [("query" :: Text, query)]
```

### 管理员路由模块 (AdminRoutes.hs)
```haskell
{-# LANGUAGE OverloadedStrings #-}

module AdminRoutes (routes) where

import Web.Scotty
import Data.Text.Lazy (Text)

-- 管理员认证中间件
adminAuth :: ActionM () -> ActionM ()
adminAuth action = do
    authHeader <- header "X-Admin-Token" `rescue` (\_ -> return Nothing)
    case authHeader of
        Just "admin-secret-token" -> action
        _ -> do
            status status403
            json [("error" :: Text, "需要管理员权限")]

routes :: ScottyM ()
routes = do
    get "/admin/stats" $ adminAuth $ 
        json [("message" :: Text, "系统统计")]
    
    get "/admin/users" $ adminAuth $ 
        json [("message" :: Text, "管理员用户列表")]
    
    post "/admin/users/:id/ban" $ adminAuth $ do
        userId <- param "id"
        json [("message" :: Text, "用户已封禁"), ("userId" :: Text, userId)]
    
    get "/admin/logs" $ adminAuth $ do
        level <- param "level" `rescue` (\_ -> return "info")
        json [("level" :: Text, level)]
```

### 高级路由模式
```haskell
-- 版本化 API
apiV1Routes :: ScottyM ()
apiV1Routes = do
    get "/api/v1/users" $ json [("version" :: Text, "1.0")]
    get "/api/v1/products" $ json [("version" :: Text, "1.0")]

apiV2Routes :: ScottyM ()
apiV2Routes = do
    get "/api/v2/users" $ json [("version" :: Text, "2.0")]
    get "/api/v2/products" $ json [("version" :: Text, "2.0")]

-- 路由组合器
combineRoutes :: [ScottyM ()] -> ScottyM ()
combineRoutes = sequence_

-- 使用示例
allRoutes :: ScottyM ()
allRoutes = combineRoutes [apiV1Routes, apiV2Routes]
```

## 最佳实践总结

### 路由设计原则
1. **RESTful 设计** - 使用标准 HTTP 方法和资源命名
2. **一致性** - 保持 URL 格式和命名约定一致
3. **可预测性** - URL 结构直观易懂
4. **版本控制** - 提供明确的 API 版本管理

### 性能优化
1. **路由顺序** - 常用路由放在前面
2. **精确匹配优先** - 静态路由优于参数路由
3. **避免深层嵌套** - 过深的 URL 影响性能

### 错误处理
1. **参数验证** - 验证所有输入参数
2. **明确错误响应** - 提供有意义的错误信息
3. **正确状态码** - 使用恰当的 HTTP 状态码

### 安全考虑
1. **输入验证** - 验证所有用户输入
2. **认证中间件** - 统一权限检查
3. **敏感信息保护** - 避免在 URL 暴露敏感数据

### 项目结构示例
```
src/
├── Main.hs              -- 主应用入口
├── UserRoutes.hs        -- 用户相关路由
├── ProductRoutes.hs     -- 产品相关路由
├── AdminRoutes.hs       -- 管理员路由
├── AuthMiddleware.hs    -- 认证中间件
└── Utils.hs             -- 工具函数
```

通过掌握这些路由设计技巧，你可以构建出清晰、高效、易维护的 Web API。