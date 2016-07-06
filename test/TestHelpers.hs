module TestHelpers where

import qualified Data.Aeson as AE
import qualified Data.Aeson.Encode.Pretty as AE
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import qualified GHC.Generics as G
import Network.JSONApi.Document
import Network.URL (URL, importURL)

prettyEncode :: AE.ToJSON a => a -> BS.ByteString
prettyEncode = AE.encodePretty' prettyConfig

prettyConfig :: AE.Config
prettyConfig = AE.Config { AE.confIndent = 2, AE.confCompare = mempty }

data TestResourceObject =
  TestResourceObject { myId :: Int
                     , myName :: Text
                     , myAge :: Int
                     , myFavoriteFood :: Text
                     } deriving (Show, G.Generic)

data TestMetaObject =
  TestMetaObject { totalPages :: Int
                 , isSuperFun :: Bool
                 } deriving (Show, G.Generic)

instance AE.ToJSON TestResourceObject
instance AE.FromJSON TestResourceObject

instance AE.ToJSON TestMetaObject
instance AE.FromJSON TestMetaObject

toResourceObject :: TestResourceObject -> ResourceObject TestResourceObject Bool
toResourceObject obj =
  ResourceObject
    (ResourceId . pack . show . myId $ obj)
    (ResourceType "TestResourceObject")
    obj
    (Just $ objectLinks obj)
    (Just $ objectMetaData obj)

objectLinks :: TestResourceObject -> Links
objectLinks obj =
  toLinks [ ("self", toURL ("/me/" <> (show $ myId obj)))
          , ("related", toURL ("/friends/" <> (show $ myId obj)))
          ]

objectMetaData :: TestResourceObject -> Meta Bool
objectMetaData obj =
   Meta . Map.fromList $ [ ("isOld", myAge obj > 50) ]

linksObj :: Links
linksObj = toLinks [ ("self", toURL "/things/1")
                   , ("related", toURL "http://some.domain.com/other/things/1")
                   ]

testObject :: TestResourceObject
testObject = TestResourceObject 1 "Fred Armisen" 51 "Pizza"

testObject2 :: TestResourceObject
testObject2 = TestResourceObject 2 "Carrie Brownstein" 35 "Lunch"

testMetaObj :: Meta TestMetaObject
testMetaObj =
  Meta . Map.fromList $ [ ("importantData", TestMetaObject 3 True) ]

emptyMeta :: Maybe (Meta TestMetaObject)
emptyMeta = Nothing

toURL :: String -> URL
toURL = fromJust . importURL

emptyLinks :: Maybe Links
emptyLinks = Nothing
