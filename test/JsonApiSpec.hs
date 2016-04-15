
module JsonApiSpec where

import JsonApi
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "gimmeFive" $ do
    it "returns 5" $ do
      gimmeFive `shouldBe` 5
