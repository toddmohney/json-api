module Network.JSONApi.ErrorSpec where

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
  describe "Defaults" $ do
    it "provides defaults" $
      let expectedDefault = Error
            { id     = Nothing
            , links  = Nothing
            , status = Nothing
            , code   = Nothing
            , title  = Nothing
            , detail = Nothing
            , meta   = Nothing
            }
      in (def::Error Int) `shouldBe` expectedDefault

  describe "JSON serialization" $
    it "provides ToJSON/FromJSON instances" $ do
      let testError = (def::Error Int)
      let encJson = BS.unpack . prettyEncode $ testError
      let decJson = AE.decode (BS.pack encJson) :: Maybe (Error Int)
      isJust decJson `shouldBe` True
      {- putStrLn encJson -}
      {- putStrLn $ show . fromJust $ decJson -}
