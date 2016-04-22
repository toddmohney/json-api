{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.JsonApiSpec where

import           Data.Aeson (ToJSON)
import qualified Data.Aeson as AE
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Maybe (isJust)
import           Network.JsonApi
import           TestHelpers
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "JSON serialization" $ do
    it "can be encoded and decoded from JSON" $ do
      let jsonApiObj = JsonApi (toResource testObject) emptyLinks emptyMeta
      let encodedJson = encodeJsonApiObject jsonApiObj
      let decodedJson = decodeJsonApiObject (BS.pack encodedJson)
      -- putStrLn encodedJson
      -- putStrLn $ show . fromJust $ decodedJson
      (isJust decodedJson) `shouldBe` True

    it "allows an optional top-level links object" $ do
      let jsonApiObj = JsonApi (toResource testObject) (Just linksObj) emptyMeta
      let encodedJson = encodeJsonApiObject jsonApiObj
      let decodedJson = decodeJsonApiObject (BS.pack encodedJson)
      -- putStrLn encodedJson
      -- putStrLn $ show . fromJust $ decodedJson
      (isJust decodedJson) `shouldBe` True

    it "allows an optional top-level meta object" $ do
      let jsonApiObj = JsonApi (toResource testObject) emptyLinks (Just testMetaObj)
      let encodedJson = encodeJsonApiObject jsonApiObj
      let decodedJson = decodeJsonApiObject (BS.pack encodedJson)
      -- putStrLn encodedJson
      -- putStrLn $ show . fromJust $ decodedJson
      (isJust decodedJson) `shouldBe` True

decodeJsonApiObject :: ByteString -> Maybe (JsonApi TestResourceObject (Maybe TestMetaObject))
decodeJsonApiObject jsonStr = AE.decode jsonStr

encodeJsonApiObject :: (ToJSON a, ToJSON b) => JsonApi a b -> String
encodeJsonApiObject = BS.unpack . prettyEncode

