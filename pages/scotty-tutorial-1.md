# 第1节：初识 Scotty - 函数式 Web 开发的优雅之路

## Scotty 框架概述和特点

Scotty 是一个轻量级的 Haskell Web 框架，灵感来自于 Ruby 的 Sinatra 框架。它以简洁、优雅的 API 设计和强大的函数式编程特性而著称，让开发者能够快速构建高性能的 Web 应用程序。

### 核心特点

**简洁性**：Scotty 提供了极其简洁的 DSL（领域特定语言），让路由定义和处理器编写变得直观易懂。一个简单的路由可能只需要几行代码就能完成。

**函数式设计**：完全拥抱 Haskell 的函数式编程范式，支持高阶函数、不可变数据结构和纯函数，使代码更加可靠和易于测试。

**类型安全**：利用 Haskell 强大的类型系统，在编译时就能捕获大部分错误，显著提高应用程序的稳定性。

**高性能**：基于高性能的 WAI（Web Application Interface）和 Warp HTTP 服务器，能够处理大量并发请求。

**中间件支持**：提供丰富的中间件生态系统，支持身份验证、日志记录、CORS 等常见功能。

**模板集成**：无缝集成多种模板引擎，如 Blaze-HTML、Lucid、Mustache 等。

### 适用场景

Scotty 特别适合构建 RESTful API、微服务、原型应用和中小型 Web 应用程序。它的轻量级特性使其成为快速开发和部署的理想选择。

## 安装配置开发环境

### 使用 Stack（推荐）

Stack 是 Haskell 的现代构建工具，提供可重现的构建环境和依赖管理。

**安装 Stack**：

```bash
# Ubuntu/Debian
curl -sSL https://get.haskellstack.org/ | sh

# macOS (使用 Homebrew)
brew install haskell-stack

# Windows
# 从 https://docs.haskellstack.org/en/stable/install_and_upgrade/ 下载安装程序
```

**验证安装**：

```bash
stack --version
```

**初始化 Stack 环境**：

```bash
stack setup
```

这将下载并安装适当版本的 GHC（Glasgow Haskell Compiler）。

### 使用 Cabal

Cabal 是 Haskell 的传统包管理工具，现在也提供了现代化的体验。

**安装 GHC 和 Cabal**：

```bash
# Ubuntu/Debian
sudo apt-get install ghc cabal-install

# macOS
brew install ghc cabal-install

# 更新包索引
cabal update
```

## 创建第一个 Scotty 项目

### 使用 Stack 创建项目

```bash
# 创建新项目
stack new my-scotty-app simple-hpack
cd my-scotty-app

# 编辑 package.yaml 文件
```

**package.yaml 配置**：

```yaml
name: my-scotty-app
version: 0.1.0.0
github: "yourusername/my-scotty-app"
license: BSD3
author: "Your Name"
maintainer: "your.email@example.com"
copyright: "2025 Your Name"

extra-source-files:
- README.md

synopsis: My first Scotty web application
category: Web

dependencies:
- base >= 4.7 && < 5
- scotty
- text
- transformers

executables:
  my-scotty-app-exe:
    main: Main.hs
    source-dirs: app
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
```

### 使用 Cabal 创建项目

```bash
# 创建项目目录
mkdir my-scotty-app
cd my-scotty-app

# 初始化 Cabal 项目
cabal init --simple
```

**修改 my-scotty-app.cabal 文件**：

```cabal
cabal-version: 2.4
name: my-scotty-app
version: 0.1.0.0
synopsis: My first Scotty web application
license: BSD-3-Clause
author: Your Name
maintainer: your.email@example.com
build-type: Simple

executable my-scotty-app
    main-is: Main.hs
    hs-source-dirs: app
    build-depends:
        base >=4.7 && <5,
        scotty,
        text,
        transformers
    default-language: Haskell2010
    ghc-options: -threaded -rtsopts -with-rtsopts=-N
```

## Hello World 应用程序

创建 `app/Main.hs` 文件：

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Data.Text.Lazy (Text)

main :: IO ()
main = do
    putStrLn "Starting Scotty server on port 3000..."
    scotty 3000 $ do
        get "/" $ do
            html "<h1>Hello, Scotty!</h1><p>Welcome to functional web development!</p>"
        
        get "/hello/:name" $ do
            name <- param "name"
            html $ "<h1>Hello, " <> name <> "!</h1>"
        
        get "/json" $ do
            json $ object ["message" .= ("Hello from Scotty!" :: Text),
                          "status" .= ("success" :: Text)]
```

### 构建和运行

**使用 Stack**：

```bash
# 构建项目
stack build

# 运行应用
stack exec my-scotty-app-exe
```

**使用 Cabal**：

```bash
# 构建项目
cabal build

# 运行应用
cabal exec my-scotty-app
```

### 测试应用程序

打开浏览器或使用 curl 测试以下端点：

```bash
# 基本路由
curl http://localhost:3000/

# 带参数的路由
curl http://localhost:3000/hello/World

# JSON 响应
curl http://localhost:3000/json
```

## 项目结构和依赖管理

### 推荐的项目结构

```
my-scotty-app/
├── app/
│   └── Main.hs              # 应用程序入口点
├── src/
│   ├── Lib.hs               # 主要库代码
│   ├── Routes/
│   │   ├── User.hs          # 用户相关路由
│   │   └── API.hs           # API 路由
│   ├── Models/
│   │   └── User.hs          # 数据模型
│   └── Utils/
│       └── Database.hs      # 数据库工具
├── test/
│   └── Spec.hs              # 测试文件
├── static/                  # 静态文件
│   ├── css/
│   ├── js/
│   └── images/
├── templates/               # 模板文件
├── package.yaml             # Stack 配置 (或 .cabal 文件)
├── stack.yaml               # Stack 项目配置
└── README.md
```

### 常用依赖包

**核心 Web 开发**：
- `scotty`: Web 框架核心
- `wai`: Web 应用程序接口
- `warp`: HTTP 服务器
- `http-types`: HTTP 类型定义

**数据处理**：
- `aeson`: JSON 处理
- `text`: 高效文本处理
- `bytestring`: 字节字符串操作

**模板和渲染**：
- `blaze-html`: HTML 生成
- `lucid`: 另一个 HTML DSL
- `mustache`: Mustache 模板

**数据库**：
- `persistent`: 类型安全的数据库抽象层
- `esqueleto`: SQL DSL
- `sqlite-simple`: SQLite 简单接口

### 依赖管理最佳实践

**版本约束**：始终为依赖包指定合理的版本约束，确保构建的可重现性。

**最小依赖原则**：只引入真正需要的依赖包，保持项目的轻量级。

**定期更新**：定期检查和更新依赖包，但要在稳定版本中进行充分测试。

**Stack 的 LTS**：使用 Stack 的 LTS（Long Term Support）版本，确保包的兼容性。

### 下一步

在下一节中，我们将深入探讨 Scotty 的路由系统，学习如何处理不同的 HTTP 方法、路径参数和查询参数，以及如何构建更复杂的 Web 应用程序结构。

通过本节的学习，您已经掌握了 Scotty 的基础知识和开发环境的搭建。现在您可以开始探索函数式 Web 开发的强大功能和优雅设计了！