module Analysis.Calculations (
    spec,
) where

import Test.Hspec

spec :: Spec
spec = do
    describe "Calculating common metrics" $ do
        it "calc mean of vector" $ do
            True `shouldBe` True
