{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.ResourceObjectSpec where

import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Maybe (isJust)
import           Data.Text (Text, pack)
import           GHC.Generics
import           Network.JsonApi
import           TestHelpers (prettyEncode)
import           Test.Hspec

data TestObject =
  TestObject { myId :: Int
             , myName :: Text
             , myAge :: Int
             , myFavoriteFood :: Text
             } deriving (Show, Generic)

instance AE.ToJSON TestObject
instance AE.FromJSON TestObject

instance ToResourceObject TestObject where
  toResource a =
    ResourceObject
      (ResourceId . pack . show . myId $ a)
      (ResourceType "TestObject")
      a

testObject :: TestObject
testObject = TestObject 1 "Fred Armisen" 49 "Pizza"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ToResourceObject" $ do
    it "can be encoded and decoded from JSON" $ do
      let encodedJson = BS.unpack . prettyEncode . toResource $ testObject
      let decodedJson = AE.decode (BS.pack encodedJson) :: Maybe (ResourceObject TestObject)
      (isJust decodedJson) `shouldBe` True
      -- putStrLn encodedJson
      -- putStrLn $ show . fromJust $ decodedJson

