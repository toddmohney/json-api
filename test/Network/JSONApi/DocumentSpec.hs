module Network.JSONApi.DocumentSpec where

import           Control.Lens ((^?))
import           Data.Aeson (ToJSON)
import qualified Data.Aeson as AE
import qualified Data.Aeson.Lens as Lens
import           Data.ByteString.Lazy.Char8 (ByteString)
{- import qualified Data.ByteString.Lazy.Char8 as BS -}
import           Data.Maybe
import qualified Network.JSONApi.Document.Success as Doc
import           Network.JSONApi.Document
import           TestHelpers
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "JSON serialization" $ do
    it "can be encoded and decoded from JSON" $ do
      let jsonApiObj = Resource (Doc.Success (toResourceObject testObject) emptyLinks emptyMeta)
      let encodedJson = encodeDocumentObject jsonApiObj
      let decodedJson = decodeDocumentObject encodedJson
      -- putStrLn (BS.unpack encodedJson)
      isJust decodedJson `shouldBe` True

    it "contains the allowable top-level keys" $ do
      let jsonApiObj = Resource (Doc.Success (toResourceObject testObject) emptyLinks emptyMeta)
      let encodedJson = encodeDocumentObject jsonApiObj
      let dataObject = encodedJson ^? Lens.key "data"
      let linksObject = encodedJson ^? Lens.key "links"
      let metaObject = encodedJson ^? Lens.key "meta"
      isJust dataObject `shouldBe` True
      isJust linksObject `shouldBe` True
      isJust metaObject `shouldBe` True


    it "allows an optional top-level links object" $ do
      let jsonApiObj = Resource (Doc.Success (toResourceObject testObject) (Just linksObj) emptyMeta)
      let encodedJson = encodeDocumentObject jsonApiObj
      let decodedJson = decodeDocumentObject encodedJson
      -- putStrLn (BS.unpack encodedJson)
      -- putStrLn $ show . fromJust $ decodedJson
      isJust decodedJson `shouldBe` True

    it "allows an optional top-level meta object" $ do
      let jsonApiObj = Resource (Doc.Success (toResourceObject testObject) emptyLinks (Just testMetaObj))
      let encodedJson = encodeDocumentObject jsonApiObj
      let decodedJson = decodeDocumentObject encodedJson
      -- putStrLn (BS.unpack encodedJson)
      -- putStrLn $ show . fromJust $ decodedJson
      isJust decodedJson `shouldBe` True

    it "allows an optional top-level meta object" $ do
      let jsonApiObj = Resource (Doc.Success (toResourceObject testObject) (Just linksObj) (Just testMetaObj))
      let encodedJson = encodeDocumentObject jsonApiObj
      let decodedJson = decodeDocumentObject encodedJson
      -- putStrLn (BS.unpack encodedJson)
      -- putStrLn $ show . fromJust $ decodedJson
      isJust decodedJson `shouldBe` True

decodeDocumentObject :: ByteString
                    -> Maybe (Document TestResourceObject (Maybe String) (Maybe TestMetaObject))
decodeDocumentObject = AE.decode

encodeDocumentObject :: (ToJSON a, ToJSON b, ToJSON c) => Document a b c -> ByteString
encodeDocumentObject = prettyEncode

