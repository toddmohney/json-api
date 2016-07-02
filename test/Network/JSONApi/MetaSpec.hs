module Network.JSONApi.MetaSpec where

import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as Map
import           Data.Maybe (isJust)
import           Network.JSONApi.Meta
import           TestHelpers (prettyEncode)
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "serialization" $
    -- is there a compelling reason to test this?
    --
    it "serializes/deserializes primitive values" $ do
      let intTestData = Meta . Map.fromList $ [ ("numData", 5 :: Int) ]
      let encIntJson = BS.unpack . prettyEncode $ intTestData
      let decIntJson = AE.decode (BS.pack encIntJson) :: Maybe (Meta Int)
      isJust decIntJson `shouldBe` True
      -- putStrLn (keys encIntJson)
      -- putStrLn $ show . fromJust $ decIntJson

      let boolTestData = Meta . Map.fromList $ [ ("boolData", True) ]
      let encBoolJson = BS.unpack . prettyEncode $ boolTestData
      let decBoolJson = AE.decode (BS.pack encBoolJson) :: Maybe (Meta Bool)
      isJust decBoolJson `shouldBe` True
      -- putStrLn (keys encBoolJson)
      -- putStrLn $ show . fromJust $ decBoolJson

