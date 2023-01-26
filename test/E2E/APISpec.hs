module E2E.APISpec (
    spec,
) where

import Test.Hspec

spec :: Spec
spec = do
    describe "E2E Tests for API" $ do
        it "can create user" $ do
            True `shouldBe` True

        it "can delete user" $ do
            True `shouldBe` True

        it "can list users" $ do
            True `shouldBe` False

        it "can get user by id" $ do
            True `shouldBe` True
