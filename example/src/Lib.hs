{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}

module Lib
    ( startApp
    ) where

import Data.Text (Text)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Network.JSONApi.Document (Document)
import Users
import qualified Users.Controller as Controller

type API = "users" :> Get '[JSON] (Document User Text Int)
      :<|> "users" :> Capture "id" Int :> Get '[JSON] (Document User Text Int)

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = Controller.usersIndex
    :<|> Controller.userShow
