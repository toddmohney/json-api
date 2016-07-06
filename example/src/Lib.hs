{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}

module Lib
    ( startApp
    ) where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Network.JSONApi.Document
  ( Document (..)
  , Links
  , Meta (..)
  )
import qualified Network.JSONApi.Document as JSONApi
import qualified Network.JSONApi.Document.Success as JSONApi
import Network.URL
import Users

type API = "users" :> Get '[JSON] (Document User Text Int)

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return usersIndex

-- A 'controller' action handler
usersIndex :: Document User Text Int
usersIndex =
  toDocument users indexLinks (indexMetaData users)

-- Builds the Links data for the 'index' action
indexLinks :: Links
indexLinks = JSONApi.toLinks [ ("self", selfLink) ]
  where
    selfLink = toURL "/users"

-- Builds the Meta data for the 'index' action
indexMetaData :: [User] -> Meta Int
indexMetaData usrs =
  Meta . Map.fromList $ [ ("user-count", length usrs) ]

toDocument :: [User] -> Links -> Meta Int -> Document User Text Int
toDocument usrs links meta =
  Resource $ JSONApi.Success
    (map toResourceObject usrs)
    (Just links)
    (Just meta)

toURL :: String -> URL
toURL = fromJust . importURL
