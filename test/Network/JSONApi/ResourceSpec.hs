module Network.JSONApi.ResourceSpec where

import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Map (Map)
import qualified Data.Map as Map
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
  describe "ToResource" $
    it "can be encoded and decoded from JSON" $ do
      let encodedJson = BS.unpack . prettyEncode $ toTestResource testObject
      let decodedJson = AE.decode (BS.pack encodedJson) :: Maybe (Resource TestObject (Maybe Int))
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

toTestResource :: TestObject -> Resource TestObject Int
toTestResource obj =
  Resource
    (Identifier (pack . show . myId $ obj) "TestObject")
    obj
    (Just myResourceLinks)
    (Just myResourceMetaData)
    (Just myResourceRelationships)

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

myResourceMetaData :: Meta Int
myResourceMetaData = Meta . Map.fromList $ [ ("extraData", 20) ]

toURL :: String -> URL
toURL = fromJust . importURL

testObject :: TestObject
testObject = TestObject 1 "Fred Armisen" 49 "Pizza"
