module Users.Actions.Show
  ( userShowSimple
  , userShowFull
  ) where

import Data.Aeson.TH
import qualified Data.Aeson as AE
import Data.Default (def)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (pack)
import Servant (Handler, ServantErr (..), throwError, err404)
import Network.JSONApi
  ( Document
  , ErrorDocument (..)
  , Error (..)
  , Links
  , Meta
  , MetaObject (..)
  , mkMeta
  )
import qualified Network.JSONApi as JSONApi
import Network.URL
import Users as U

data DocumentMetaData = Pagination
  { currentPage :: Int
  , totalPages  :: Int
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''DocumentMetaData)

instance MetaObject DocumentMetaData where
  typeName _ = "pagination"




-- A 'controller' action handler
userShowSimple :: Int -> Handler (Document User)
userShowSimple userId = case U.getUser userId of
  Nothing -> throwError (resourceNotFound userId)
  (Just user) -> return $ showSimpleResourceDocument user


-- A 'controller' action handler
userShowFull :: Int -> Handler (Document User)
userShowFull userId = case getUser userId of
  Nothing -> throwError (resourceNotFound userId)
  (Just user) -> return $ showFullResourceDocument user (showLinks userId) documentMetaData


-- Builds the Links data for the 'show' action
showLinks :: Int -> Links
showLinks userId = JSONApi.mkLinks [ ("self", selfLink) ]
  where
    selfLink = toURL ("/users/" <> (show userId))


documentMetaData :: Meta
documentMetaData = mkMeta (Pagination { currentPage = 1, totalPages = 40})


-- Builds a simple repsonse Document for our User resource
showSimpleResourceDocument :: User -> Document User
showSimpleResourceDocument user =
  JSONApi.mkDocument
    [user]
    Nothing
    Nothing


-- Builds a full repsonse Document for our User resource
showFullResourceDocument :: User -> Links -> Meta -> Document User
showFullResourceDocument user links metaData =
  JSONApi.mkDocument
    [user]
    (Just links)
    (Just metaData)


toURL :: String -> URL
toURL = fromJust . importURL


-- Provides 404 response
resourceNotFound :: Int -> ServantErr
resourceNotFound resourceId = err404 { errBody = AE.encode errorDocument }
  where
    errorDocument :: ErrorDocument Int
    errorDocument = ErrorDocument errorObj (Just (showLinks resourceId)) Nothing

    errorObj :: Error Int
    errorObj =
      def { status = Just "404"
          , title  = Just "Resource Not Found"
          , detail = Just $ "There is no User with id: " <> (pack . show $ resourceId)
          }
