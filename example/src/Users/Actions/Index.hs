module Users.Actions.Index
  ( usersIndex
  ) where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import Servant (Handler)

import Network.JSONApi.Document
  ( Document (..)
  , ResourceData (..)
  , Links
  , Meta (..)
  )
import qualified Network.JSONApi.Document as JSONApi
import Network.URL
import Emails
import Users

-- A 'controller' action handler
usersIndex :: Handler (Document User Text Int)
usersIndex =
  return $ indexDocument usersWithEmail indexLinks (indexMetaData usersWithEmail)
  where
    usersWithEmail =
      [ (User 1 "Isaac" "Newton", Email 42 1 "isaac@newton.com")
      , (User 2 "Albert" "Einstein", Email 88 2 "albert@einstein.com")
      ]

-- Builds the Links data for the 'index' action
indexLinks :: Links
indexLinks = JSONApi.toLinks [ ("self", selfLink) ]
  where
    selfLink = toURL "/users"

-- Builds the Meta data for the 'index' action
indexMetaData :: [a] -> Meta Int
indexMetaData usrs =
  Meta . Map.fromList $ [ ("user-count", length usrs) ]

-- Builds the repsonse Document for the 'index' action
indexDocument :: [(User, Email)] -> Links -> Meta Int -> Document User Text Int
indexDocument usersWithEmail links meta =
  Document
    (List (map toResource usersWithEmail))
    (Just links)
    (Just meta)

toURL :: String -> URL
toURL = fromJust . importURL
