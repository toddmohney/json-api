{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.ResourceObjectSpec where

import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as Map
import           Data.Maybe (isJust, fromJust)
import           Data.Text (Text, pack)
import           GHC.Generics
import           Network.JsonApi
import           Network.Meta
import           Network.URL (URL, importURL)
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

toResourceObject :: TestObject -> ResourceObject TestObject Int
toResourceObject obj =
  ResourceObject
    (ResourceId . pack . show . myId $ obj)
    (ResourceType "TestObject")
    obj
    (Just resourceObjectLinks)
    (Just resourceObjectMetaData)

resourceObjectLinks :: Links
resourceObjectLinks =
  toLinks [ ("self", toURL "/me")
          , ("related", toURL "/tacos/4")
          ]

resourceObjectMetaData :: Meta Int
resourceObjectMetaData = Meta . Map.fromList $ [ ("extraData", 20) ]

toURL :: String -> URL
toURL = fromJust . importURL

testObject :: TestObject
testObject = TestObject 1 "Fred Armisen" 49 "Pizza"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ToResourceObject" $ do
    it "can be encoded and decoded from JSON" $ do
      let encodedJson = BS.unpack . prettyEncode $ toResourceObject testObject
      let decodedJson = AE.decode (BS.pack encodedJson) :: Maybe (ResourceObject TestObject (Maybe Int))
      (isJust decodedJson) `shouldBe` True
      {- putStrLn encodedJson -}
      -- putStrLn $ show . fromJust $ decodedJson

