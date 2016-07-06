module Users.Actions.Show
  ( userShow
  ) where

import qualified Data.Aeson as AE
import Data.Default (def)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Servant (Handler, ServantErr (..), throwError, err404)

import Network.JSONApi.Document
  ( Document (..)
  , ErrorDocument (..)
  , Error (..)
  , Links
  , Resource (..)
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

userShow userId = throwError (resourceNotFound userId)


resourceNotFound :: Int -> ServantErr
resourceNotFound resourceId = err404 { errBody = AE.encode errorDocument }
  where
    errorDocument :: ErrorDocument Int Int
    errorDocument = ErrorDocument errorObj (Just (showLinks resourceId)) Nothing

    errorObj :: Error Int
    errorObj =
      def { status = Just "404"
          , title  = Just "Resource Not Found"
          , detail = Just $ "There is no User with id: " <> (pack . show $ resourceId)
          }

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
