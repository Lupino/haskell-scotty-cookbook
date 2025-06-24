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
        name <- pathParam "name"
        text $ "Hello, " <> name <> "!"

    -- JSON 响应
    get "/api/status" $ json $ object
        [ "status" .= ("ok" :: Text)
        , "service" .= ("hello-scotty" :: Text)
        ]
