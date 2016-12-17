module DistanceSpec (main, spec) where

import Test.Hspec
import Distance

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = hspec $ do
  describe "Distance" $ do
    it "euclidean distance should return euclidean distance between two vectors" $ do
      Distance.euclidean [1, 2] [4, 6] `shouldBe` 5

    it "manhattan distance should return manhattan distance between two vectors" $ do
      Distance.manhattan [0, 0] [2, 2] `shouldBe` 4
