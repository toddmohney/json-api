module Network.JSONApi.MetaSpec where

import           Data.Aeson (ToJSON)
import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (isJust)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Network.JSONApi.Meta
import           TestHelpers (prettyEncode)
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "JSON serialization" $ do
    it "serializes/deserializes maps simple heterogeneous values" $ do
      let testMeta = Meta . HM.fromList $ [ ("numData", AE.toJSON (5 :: Int))
                                          , ("strData", AE.toJSON ("hello" :: String))
                                          ]
      let encIntJson = BS.unpack . prettyEncode $ testMeta
      let decIntJson = AE.decode (BS.pack encIntJson) :: Maybe Meta
      isJust decIntJson `shouldBe` True

    it "serializes/deserializes heterogeneous maps of ToJSON types" $ do
      let boolTestData = Meta . HM.fromList $ [ ("objData", AE.toJSON testObject)
                                              , ("otherObjData", AE.toJSON otherTestObject)
                                              ]
      let encBoolJson = BS.unpack . prettyEncode $ boolTestData
      let decBoolJson = AE.decode (BS.pack encBoolJson) :: Maybe Meta
      isJust decBoolJson `shouldBe` True

  describe "monoid instance" $
    it "combines" $ do
      let monoidialConstruction = mkMeta testObject <> mkMeta otherTestObject
      let manualConstruction = Meta . HM.fromList $ [ ("testObject", AE.toJSON testObject)
                                                    , ("otherTestObject", AE.toJSON otherTestObject)
                                                    ]
      monoidialConstruction `shouldBe` manualConstruction

testObject :: TestObject
testObject = TestObject 99 102 "Zapp Brannigan"

otherTestObject :: OtherTestObject
otherTestObject = OtherTestObject "Olive Garden" "Woofers" 29 "TGIFriday's"

data TestObject = TestObject
  { myID :: Int
  , myAge :: Int
  , myName :: Text
  } deriving (Show, Generic)

instance ToJSON TestObject
instance MetaObject TestObject where
  typeName _ = "testObject"

data OtherTestObject = OtherTestObject
  { myFavoriteRestaurant :: Text
  , myDogsName :: Text
  , myDogsAge :: Int
  , myDogsFavoriteRestarant :: Text
  } deriving (Show, Generic)

instance ToJSON OtherTestObject
instance MetaObject OtherTestObject where
  typeName _ = "otherTestObject"
