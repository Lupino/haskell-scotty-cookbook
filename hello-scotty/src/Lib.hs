{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( someFunc
  ) where

import           Control.Exception         (SomeException)
import           Data.Aeson                (FromJSON, ToJSON, object, (.=))
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as L
import           Data.Text.Lazy            (Text, pack)
import           GHC.Generics
import           Network.HTTP.Types.Status
import           Network.Wai.Parse         (fileContent, fileName)
import           Text.Read                 (readMaybe)
import           Web.Scotty

data User = User
    { userName  :: Text
    , userEmail :: Text
    } deriving (Generic, Show)

instance FromJSON User
instance ToJSON User

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
    name <- pathParam "name"
    text $ "Hello, " <> name <> "!"

  -- JSON 响应
  get "/api/status" $ json $ object
    [ "status" .= ("ok" :: Text)
    , "service" .= ("hello-scotty" :: Text)
    ]

  -- POST 请求
  post "/some_path" $ text "This is a POST method to /some_path"

  -- PUT 请求
  put "/some_path" $ text "This is a PUT method to /some_path"

  -- DELETE 请求
  delete "/some_path" $ text "This is a DELETE method to /some_path"


  -- URL: /users/123/posts/456
  get "/users/:userId/posts/:postId" $ do
    userId <- pathParam "userId"
    postId <- pathParam "postId"
    text $ "User: " <> userId <> ", Post: " <> postId

  -- URL: /search?q=haskell&limit=10
  get "/search" $ do
    query <- queryParam "q"           -- 必需参数
    limit <- queryParam "limit" `catch` (\(_ :: SomeException) -> return "20")  -- 可选参数
    text $ "Query: " <> query <> ", Limit: " <> limit

  get "/users/:id" $ do
    idText <- pathParam "id"
    case readMaybe idText of
      Nothing -> do
        status status400
        text "Invalid user ID"
      Just userId -> do
        -- 处理有效的用户ID
        text $ "User ID: " <> pack (show (userId :: Int))

  post "/login" $ do
    username <- formParam "username" :: ActionM Text
    password <- formParam "password" :: ActionM Text
    -- 验证逻辑
    if username == "admin" && password == "secret"
      then text "Login successful"
      else do
        status status401
        text "Invalid credentials"

  post "/users" $ do
    user <- jsonData :: ActionM User
    liftIO $ print user  -- 处理用户数据
    json user  -- 返回创建的用户

  post "/upload" $ do
      inFiles <- files
      case inFiles of
          [] -> text "No file uploaded"
          ((_, fileInfo):_) -> do
              let content = fileContent fileInfo
                  name = fileName fileInfo
              liftIO $ L.writeFile ("uploads/" ++ BC.unpack name) content
              text $ "File uploaded: " <> pack (BC.unpack name)
