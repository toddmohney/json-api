module Users.Actions.Show
  ( userShow
  ) where

import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import Servant (Handler)

import Network.JSONApi.Document
  ( Document (..)
  , Resource (..)
  , Links
  )
import qualified Network.JSONApi.Document as JSONApi
import Network.URL
import Users

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
