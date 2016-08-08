module TestHelpers where

import qualified Data.Aeson as AE
import qualified Data.Aeson.Encode.Pretty as AE
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromJust)
import qualified Data.HashMap.Strict as HM
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import qualified GHC.Generics as G
import Network.JSONApi.Document
import Network.URL (URL, importURL)

prettyEncode :: AE.ToJSON a => a -> BS.ByteString
prettyEncode = AE.encodePretty' prettyConfig

prettyConfig :: AE.Config
prettyConfig = AE.Config { AE.confIndent = 2, AE.confCompare = mempty }

class HasIdentifiers a where
  uniqueId :: a -> Int
  typeDescriptor :: a -> Text

data TestResource = TestResource
  { myId :: Int
  , myName :: Text
  , myAge :: Int
  , myFavoriteFood :: Text
  } deriving (Show, G.Generic)

data OtherTestResource = OtherTestResource
  { myFavoriteNumber :: Int
  , myJob :: Text
  , myPay :: Int
  , myEmployer :: Text
  } deriving (Show, G.Generic)

data TestMetaObject = TestMetaObject
  { totalPages :: Int
  , isSuperFun :: Bool
  } deriving (Show, G.Generic)

instance AE.ToJSON TestResource
instance AE.FromJSON TestResource
instance HasIdentifiers TestResource where
  uniqueId = myId
  typeDescriptor _ = "TestResource"

instance AE.ToJSON OtherTestResource
instance AE.FromJSON OtherTestResource
instance HasIdentifiers OtherTestResource where
  uniqueId = myFavoriteNumber
  typeDescriptor _ = "OtherTestResource"

instance AE.ToJSON TestMetaObject
instance AE.FromJSON TestMetaObject

toResource :: TestResource -> Resource TestResource
toResource obj =
  Resource
    (Identifier (pack . show . myId $ obj) "TestResource")
    obj
    (Just $ objectLinks obj)
    (Just $ objectMetaData obj)
    Nothing

toResource' :: (HasIdentifiers a) => a
            -> Maybe Links
            -> Maybe Meta
            -> Resource a
toResource' obj links meta =
  Resource
    (Identifier (pack . show . uniqueId $ obj) (typeDescriptor obj))
    obj
    links
    meta
    Nothing

objectLinks :: TestResource -> Links
objectLinks obj =
  toLinks [ ("self", toURL ("/me/" <> (show $ myId obj)))
          , ("related", toURL ("/friends/" <> (show $ myId obj)))
          ]

objectMetaData :: TestResource -> Meta
objectMetaData obj =
   Meta . HM.fromList $ [ ("isOld", (AE.toJSON $ myAge obj > 50)) ]

linksObj :: Links
linksObj = toLinks [ ("self", toURL "/things/1")
                   , ("related", toURL "http://some.domain.com/other/things/1")
                   ]

testObject :: TestResource
testObject = TestResource 1 "Fred Armisen" 51 "Pizza"

testObject2 :: TestResource
testObject2 = TestResource 2 "Carrie Brownstein" 35 "Lunch"

otherTestObject :: OtherTestResource
otherTestObject = OtherTestResource 999 "Atom Smasher" 100 "Atom Smashers, Inc"

testMetaObj :: Meta
testMetaObj =
  Meta . HM.fromList $ [ ("importantData", (AE.toJSON $ TestMetaObject 3 True)) ]

emptyMeta :: Maybe Meta
emptyMeta = Nothing

toURL :: String -> URL
toURL = fromJust . importURL

emptyLinks :: Maybe Links
emptyLinks = Nothing
