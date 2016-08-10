module Network.JSONApi.DocumentSpec where

import Control.Lens ((^?))
import Data.Aeson (ToJSON)
import qualified Data.Aeson as AE
import qualified Data.Aeson.Lens as Lens
import Data.ByteString.Lazy.Char8 (ByteString)
{- import qualified Data.ByteString.Lazy.Char8 as BS -}
import Data.Either (isRight)
import Data.Maybe
import Data.Monoid
import Network.JSONApi
import TestHelpers
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "JSON serialization" $ do
    it "JSON encodes/decodes a singleton resource" $ do
      -- TODO: test the main resource actually is a singleton
      let jsonApiObj = mkDocument [testObject] Nothing Nothing
      let encodedJson = encodeDocumentObject jsonApiObj
      let decodedJson = decodeDocumentObject encodedJson
      {- putStrLn (BS.unpack encodedJson) -}
      {- putStrLn (show decodedJson) -}
      isRight decodedJson `shouldBe` True

    it "JSON encodes/decodes a list of resources" $ do
      -- TODO: test the main resource actually is a list
      let jsonApiObj = mkDocument [testObject, testObject2] Nothing Nothing
      let encodedJson = encodeDocumentObject jsonApiObj
      let decodedJson = decodeDocumentObject encodedJson
      {- putStrLn (BS.unpack encodedJson) -}
      {- putStrLn (show decodedJson) -}
      isRight decodedJson `shouldBe` True

    it "contains the allowable top-level keys" $ do
      let jsonApiObj = mkDocument [testObject] Nothing Nothing
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
      let jsonApiObj = mkDocument [testObject] (Just linksObj) Nothing
      let encodedJson = encodeDocumentObject jsonApiObj
      let decodedJson = decodeDocumentObject encodedJson
      -- putStrLn (BS.unpack encodedJson)
      -- putStrLn $ show decodedJson
      isRight decodedJson `shouldBe` True

    it "allows an optional top-level meta object" $ do
      let jsonApiObj = mkDocument [testObject] Nothing (Just testMetaObj)
      let encodedJson = encodeDocumentObject jsonApiObj
      let decodedJson = decodeDocumentObject encodedJson
      -- putStrLn (BS.unpack encodedJson)
      -- putStrLn $ show decodedJson
      isRight decodedJson `shouldBe` True

    it "allows a heterogeneous list of related resources" $ do
      let includedResources = (mkIncludedResource testObject) <> (mkIncludedResource testObject2)
      let jsonApiObj = mkCompoundDocument [testObject] Nothing Nothing includedResources
      let encodedJson = encodeDocumentObject jsonApiObj
      let decodedJson = decodeDocumentObject encodedJson
      {- putStrLn (BS.unpack encodedJson) -}
      {- putStrLn $ show decodedJson -}
      isRight decodedJson `shouldBe` True

decodeDocumentObject :: ByteString
                    -> Either String (Document TestResource)
decodeDocumentObject = AE.eitherDecode

encodeDocumentObject :: (ToJSON a) => Document a -> ByteString
encodeDocumentObject = prettyEncode
