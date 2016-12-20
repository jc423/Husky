module DistanceSpec (main, spec) where

import Test.Hspec
import Distance

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Distance" $ do
    it "euclidean distance should return euclidean distance between two vectors for positive values" $ do
      Distance.euclidean [1::Int, 2] [4, 6] `shouldBe` 5.0

    it "euclidean distance should return euclidean distance between two vectors for negative values" $ do
      Distance.euclidean [-11::Int, -14] [-5, -6] `shouldBe` 10.0

    it "euclidean distance should return euclidean distance between two vectors for mix of positive and negative values" $ do
      Distance.euclidean [-1::Int, 2] [7, 8] `shouldBe` 10.0

    it "manhattan distance should return manhattan distance between two vectors for positive values" $ do
      Distance.manhattan [0::Int, 0] [2, 2] `shouldBe` 4.0

    it "manhattan distance should return manhattan distance between two vectors for negative values" $ do
      Distance.manhattan [-4::Int, -3] [-2, -1] `shouldBe` 4.0

    it "manhattan distance should return manhattan distance between two vectors for mix of positive and negative values" $ do
      Distance.manhattan [-1::Int, 0] [2, -4] `shouldBe` 7.0

    it "cosine distance should return absolute value of 1 - cosine similarity between two vectors for positive values" $ do
      Distance.cosineDistance [0::Int, 1] [0, 2] `shouldBe` 0.0

    it "cosine distance should return absolute value of 1 - cosine similarity between two vectors for negative values" $ do
      Distance.cosineDistance [-1::Int, -1] [-3, -3] `shouldBe` 0.0

    it "cosine distance should return absolute value of 1 - cosine similarity between two vectors for mix of positive  and negative values" $ do
      Distance.cosineDistance [1.0::Double, 1.0] [-1.0, -1.0] - 2.0 `shouldSatisfy` (< 0.0000001)

