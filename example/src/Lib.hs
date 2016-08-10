{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}

module Lib
    ( startApp
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.JSONApi.Document (Document)
import Servant
import Users
import qualified Users.Controller as Controller

type API = "users" :> Get '[JSON] (Document User)
      :<|> "users" :> Capture "id" Int :> "simple" :> Get '[JSON] (Document User)

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = Controller.usersIndex
    :<|> Controller.userShow
