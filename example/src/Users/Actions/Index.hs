module Users.Actions.Index
  ( usersIndex
  ) where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import Servant (Handler)

import Network.JSONApi.Document
  ( Document (..)
  , Resource (..)
  , Links
  , Meta (..)
  )
import qualified Network.JSONApi.Document as JSONApi
import Network.URL
import Users

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

toURL :: String -> URL
toURL = fromJust . importURL
