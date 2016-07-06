{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}

module Lib
    ( startApp
    ) where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Network.JSONApi.Document
  ( Document (..)
  , Resource (..)
  , Links
  , Meta (..)
  )
import qualified Network.JSONApi.Document as JSONApi
import Network.URL
import Users

type API = "users" :> Get '[JSON] (Document User Text Int)
      :<|> "users" :> Capture "id" Int :> Get '[JSON] (Document User Text Int)

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = usersIndex
    :<|> userShow

-- Index
-------------------------------------------------------------------------

-- A 'controller' action handler
usersIndex :: Handler (Document User Text Int)
usersIndex =
  return $ indexDocument users indexLinks (indexMetaData users)
  where
    users =
      [ User 1 "Isaac" "Newton"
      , User 2 "Albert" "Einstein"
      ]

-- Builds the Links data for the 'index' action
indexLinks :: Links
indexLinks = JSONApi.toLinks [ ("self", selfLink) ]
  where
    selfLink = toURL "/users"

-- Builds the Meta data for the 'index' action
indexMetaData :: [User] -> Meta Int
indexMetaData usrs =
  Meta . Map.fromList $ [ ("user-count", length usrs) ]

-- Builds the repsonse Document for the 'index' action
indexDocument :: [User] -> Links -> Meta Int -> Document User Text Int
indexDocument usrs links meta =
  Document
    (List (map toResourceObject usrs))
    (Just links)
    (Just meta)

-------------------------------------------------------------------------

-- Show
-------------------------------------------------------------------------

userShow :: Int -> Handler (Document User Text Int)
userShow 1 =
  let user = User 1 "Isaac" "Newton"
  in return $ showDocument user (showLinks 1)

userShow 2 =
  let user = User 2 "Albert" "Einstein"
  in return $ showDocument user (showLinks 2)

userShow _ = undefined

-- Builds the Links data for the 'show' action
showLinks :: Int -> Links
showLinks userId = JSONApi.toLinks [ ("self", selfLink) ]
  where
    selfLink = toURL ("/users/" <> (show userId))

-- Builds the repsonse Document for the 'show' action
showDocument :: User -> Links -> Document User Text Int
showDocument usr links =
  Document
    (Singleton $ toResourceObject usr)
    (Just links)
    Nothing

toURL :: String -> URL
toURL = fromJust . importURL
