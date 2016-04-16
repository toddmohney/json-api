{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ResourceObjectSpec where

import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (isJust, fromJust)
import Data.Text (Text, pack)
import GHC.Generics
import JsonApi
import Test.Hspec

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
      let encodedJson = BS.unpack . AE.encode . toResource $ testObject
      putStrLn encodedJson
      let decodedJson = AE.decode (BS.pack encodedJson) :: Maybe (ResourceObject TestObject)
      putStrLn $ show . fromJust $ decodedJson
      (isJust decodedJson) `shouldBe` True

