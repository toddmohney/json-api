module Network.JSONApi.DocumentSpec where

import           Control.Lens ((^?))
import           Data.Aeson (ToJSON)
import qualified Data.Aeson as AE
import qualified Data.Aeson.Lens as Lens
import           Data.ByteString.Lazy.Char8 (ByteString)
{- import qualified Data.ByteString.Lazy.Char8 as BS -}
import           Data.Either (isRight)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Text
import           Network.JSONApi.Document
import           TestHelpers
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "JSON serialization" $ do
    it "JSON encodes/decodes a singleton resource" $ do
      let resource = toResource testObject
      let jsonApiObj = Document (Singleton resource) emptyLinks emptyMeta []
      let encodedJson = encodeDocumentObject jsonApiObj
      let decodedJson = decodeDocumentObject encodedJson
      {- putStrLn (BS.unpack encodedJson) -}
      {- putStrLn (show decodedJson) -}
      isRight decodedJson `shouldBe` True

    it "JSON encodes/decodes a list of resources" $ do
      let resources = [toResource testObject, toResource testObject2]
      let jsonApiObj = Document (List resources) emptyLinks emptyMeta []
      let encodedJson = encodeDocumentObject jsonApiObj
      let decodedJson = decodeDocumentObject encodedJson
      {- putStrLn (BS.unpack encodedJson) -}
      {- putStrLn (show decodedJson) -}
      isRight decodedJson `shouldBe` True

    it "contains the allowable top-level keys" $ do
      let resource = toResource testObject
      let jsonApiObj = Document (Singleton resource) emptyLinks emptyMeta []
      let encodedJson = encodeDocumentObject jsonApiObj
      let dataObject = encodedJson ^? Lens.key "data"
      let linksObject = encodedJson ^? Lens.key "links"
      let metaObject = encodedJson ^? Lens.key "meta"
      let includedObject = encodedJson ^? Lens.key "included"
      isJust dataObject `shouldBe` True
      isJust linksObject `shouldBe` True
      isJust metaObject `shouldBe` True
      isJust includedObject `shouldBe` True

    it "allows an optional top-level links object" $ do
      let resource = toResource testObject
      let jsonApiObj = Document (Singleton resource) (Just linksObj) emptyMeta []
      let encodedJson = encodeDocumentObject jsonApiObj
      let decodedJson = decodeDocumentObject encodedJson
      -- putStrLn (BS.unpack encodedJson)
      -- putStrLn $ show decodedJson
      isRight decodedJson `shouldBe` True

    it "allows an optional top-level meta object" $ do
      let resource = toResource testObject
      let jsonApiObj = Document (Singleton resource) emptyLinks (Just testMetaObj) []
      let encodedJson = encodeDocumentObject jsonApiObj
      let decodedJson = decodeDocumentObject encodedJson
      -- putStrLn (BS.unpack encodedJson)
      -- putStrLn $ show decodedJson
      isRight decodedJson `shouldBe` True

    it "allows an optional top-level meta object" $ do
      let resource = toResource testObject
      let jsonApiObj = Document (Singleton resource) (Just linksObj) (Just testMetaObj) []
      let encodedJson = encodeDocumentObject jsonApiObj
      let decodedJson = decodeDocumentObject encodedJson
      -- putStrLn (BS.unpack encodedJson)
      -- putStrLn $ show decodedJson
      isRight decodedJson `shouldBe` True

    it "allows a heterogeneous list of related resources" $ do
      let resource = toResource testObject
      let jsonApiObj = Document (Singleton resource) emptyLinks emptyMeta includedResources
      let encodedJson = encodeDocumentObject jsonApiObj
      let decodedJson = decodeDocumentObject encodedJson
      {- putStrLn (BS.unpack encodedJson) -}
      {- putStrLn $ show decodedJson -}
      isRight decodedJson `shouldBe` True

decodeDocumentObject :: ByteString
                    -> Either String (Document TestResource (Maybe Bool) (Maybe TestMetaObject))
decodeDocumentObject = AE.eitherDecode

encodeDocumentObject :: (ToJSON a, ToJSON b, ToJSON c) => Document a b c -> ByteString
encodeDocumentObject = prettyEncode

includedResources :: [ Map Text (Resource TestResource Bool) ]
includedResources = [ Map.singleton "TestResource" (toResource testObject)
                    ]

