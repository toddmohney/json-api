module Network.JSONApi.IdentifierSpec where

import Control.Lens ((^.))
import Network.JSONApi.Identifier
import Test.Hspec
import TestHelpers (testMetaObj)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Lenses" $ do
    it "provides property access via lens" $ do
      testIdentifier ^. ident `shouldBe` "3"
      testIdentifier ^. datatype `shouldBe` "SomeIdentifier"
      testIdentifier ^. metadata `shouldBe` Just testMetaObj

testIdentifier :: Identifier
testIdentifier = Identifier "3" "SomeIdentifier" (Just testMetaObj)
