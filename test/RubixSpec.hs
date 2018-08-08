module RubixSpec where

import Rubix

import Test.Hspec
import Data.List (elemIndex)
import Data.Maybe (fromJust)

spec :: Spec
spec = do
  describe "Test repeated moves" $ do
    it "turnF" $ do
      (solved `apply` turnF . turnF . turnF . turnF) `shouldBe` solved
    it "turnU" $ do
      (solved `apply` turnU . turnU . turnU . turnU) `shouldBe` solved
    it "turnR" $ do
      (solved `apply` turnR . turnR . turnR . turnR) `shouldBe` solved
    it "turnD" $ do
      (solved `apply` turnD . turnD . turnD . turnD) `shouldBe` solved
    it "turnL" $ do
      (solved `apply` turnL . turnL . turnL . turnL) `shouldBe` solved
    it "turnB" $ do
      (solved `apply` turnB . turnB . turnB . turnB) `shouldBe` solved
    it "turnF . turnL" $ do
      ((iterate (turnF . turnL) solved) !! 126) `shouldBe` solved

