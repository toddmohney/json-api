module Network.JSONApi.SourceSpec where

import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Default (def)
import Data.Maybe
import Network.JSONApi
import Prelude hiding (id)
import TestHelpers (prettyEncode)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Defaults" $
    it "provides defaults" $
      let expectedDefault = Source
            { pointer   = Nothing
            , parameter = Nothing
            }
      in (def::Source) `shouldBe` expectedDefault

  describe "JSON serialization" $
    it "provides ToJSON/FromJSON instances" $ do
      let testSource = def::Source
      let encJson = BS.unpack . prettyEncode $ testSource
      let decJson = AE.decode (BS.pack encJson) :: Maybe Source

      isJust decJson `shouldBe` True

      -- putStrLn encJson
      -- print (fromJust decJson)
