module Users
  ( User (..)
  , UserMetaData (..)
  , toResource
  , getUsers
  , getUser
  ) where

import Data.Aeson.TH
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (pack)
import Network.URL
import Network.JSONApi
  ( Links
  , Meta
  , MetaObject (..)
  , ResourcefulEntity (..)
  , mkMeta
  )
import qualified Network.JSONApi as JSONApi

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

data UserMetaData = UserMetaData
  { count :: Int
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''UserMetaData)

instance MetaObject UserMetaData where
  typeName _ = "userCount"

instance ResourcefulEntity User where
  resourceIdentifier = pack . show . userId
  resourceType _ = "User"
  resourceLinks = Just . userLinks
  resourceMetaData _ = Just userMetaData
  resourceRelationships _ = Nothing

-- helper function to build links for a User resource
userLinks :: User -> Links
userLinks user = JSONApi.mkLinks [ ("self", selfLink) ]
  where
    selfLink = toURL selfPath
    selfPath = "/users/" <> (show $ userId user)

userMetaData :: Meta
userMetaData = mkMeta (UserMetaData $ length getUsers)

toURL :: String -> URL
toURL = fromJust . importURL

getUser :: Int -> Maybe User
getUser 1 = Just isacc
getUser 2 = Just albert
getUser _ = Nothing

getUsers :: [User]
getUsers = [isacc, albert]

isacc :: User
isacc = User 1 "Isaac" "Newton"

albert :: User
albert = User 2 "Albert" "Einstein"
