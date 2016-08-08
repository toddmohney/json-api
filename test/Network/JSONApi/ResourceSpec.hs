module Network.JSONApi.ResourceSpec where

import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM
import Data.Maybe (isJust, fromJust)
import Data.Text (Text, pack)
import qualified GHC.Generics as G
import Network.JSONApi.Resource
import Network.JSONApi.Link
import Network.JSONApi.Meta
import Network.URL (URL, importURL)
import TestHelpers (prettyEncode)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "JSON serialization" $
    it "can be encoded and decoded from JSON" $ do
      let encodedJson = BS.unpack . prettyEncode $ toResource testObject
      let decodedJson = AE.decode (BS.pack encodedJson) :: Maybe (Resource TestObject)
      isJust decodedJson `shouldBe` True
      {- putStrLn encodedJson -}
      {- putStrLn $ show . fromJust $ decodedJson -}

data TestObject = TestObject
  { myId :: Int
  , myName :: Text
  , myAge :: Int
  , myFavoriteFood :: Text
  } deriving (Show, G.Generic)

instance AE.ToJSON TestObject
instance AE.FromJSON TestObject
instance ResourcefulEntity TestObject where
  resourceIdentifier = pack . show . myId
  resourceType _ = "TestObject"
  resourceLinks _ = Just myResourceLinks
  resourceMetaData _ = Just myResourceMetaData
  resourceRelationships _ = Just myResourceRelationships

myResourceRelationships :: Map Text Relationship
myResourceRelationships = Map.fromList $ [ ("friends", relationship) ]

relationship :: Relationship
relationship =
  fromJust $ mkRelationship
    (Just $ Identifier "42" "FriendOfTestObject")
    (Just myResourceLinks)

myResourceLinks :: Links
myResourceLinks =
  toLinks [ ("self", toURL "/me")
          , ("related", toURL "/tacos/4")
          ]

myResourceMetaData :: Meta
myResourceMetaData = Meta . HM.fromList $ [ ("extraData", AE.toJSON (20 :: Int)) ]

toURL :: String -> URL
toURL = fromJust . importURL

testObject :: TestObject
testObject = TestObject 1 "Fred Armisen" 49 "Pizza"
