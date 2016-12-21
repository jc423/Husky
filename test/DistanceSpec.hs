module DistanceSpec (main, spec) where

import Test.Hspec
import Control.Exception (evaluate)
import Distance
import HuskyML

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Distance" $ do
    it "euclidean distance should throw when two vectors have mismatched corresponding types" $ do
      evaluate (Distance.euclidean [FInt 1, FChar 'b'] [FInt 4, FInt 6]) `shouldThrow` anyException
    
    it "euclidean distance should return euclidean distance between two vectors for positive values" $ do
      Distance.euclidean [FInt 1, FInt 2] [FInt 4, FInt 6] `shouldBe` 5.0

    it "euclidean distance should return euclidean distance between two vectors for negative values" $ do
      Distance.euclidean [FInt (-11), FInt (-14)] [FInt (-5), FInt (-6)] `shouldBe` 10.0

    it "euclidean distance should return euclidean distance between two vectors for mix of positive and negative values" $ do
      Distance.euclidean [FInt (-1),FInt 2] [FInt 7, FInt 8] `shouldBe` 10.0

    it "euclidean distance should return euclidean distance between two vectors for mixed feature types" $ do
      Distance.euclidean [FInt 1, FString "jo", FChar 'a', FDouble 1.0 ] [FInt 4, FString "crawfo", FChar 'a', FDouble 1.0] `shouldBe` 5.0

    it "manhattan distance should return manhattan distance between two vectors for positive values" $ do
      Distance.manhattan [FInt 0, FInt 0] [FInt 2,FInt 2] `shouldBe` 4.0

    it "manhattan distance should return manhattan distance between two vectors for negative values" $ do
      Distance.manhattan [FInt (-4),FInt (-3)] [FInt (-2),FInt (-1)] `shouldBe` 4.0

    it "manhattan distance should return manhattan distance between two vectors for mix of positive and negative values" $ do
      Distance.manhattan [FInt (-1),FInt 0] [FInt 2,FInt (-4)] `shouldBe` 7.0

    it "manhattan distance should return manhattan distance between two vectors for mixed feature types" $ do
      Distance.manhattan [FChar 'd',FInt 0] [FChar 'a',FInt (-4)] `shouldBe` 7.0

    it "cosine distance should return absolute value of 1 - cosine similarity between two vectors for positive values" $ do
      Distance.cosineDistance [FInt 0,FInt 1] [FInt 0,FInt 2] `shouldBe` 0.0

    it "cosine distance should return absolute value of 1 - cosine similarity between two vectors for negative values" $ do
      Distance.cosineDistance [FInt (-1),FInt (-1)] [FInt (-3),FInt (-3)] `shouldBe` 0.0

    it "cosine distance should return absolute value of 1 - cosine similarity between two vectors for mix of positive  and negative values" $ do
      Distance.cosineDistance [FInt 1,FInt 1] [FInt (-1),FInt (-1)] - 2.0 `shouldSatisfy` (< 0.0000001)

    it "cosine distance should return absolute value of 1 - cosine similarity between two vectors for mixed feature types" $ do
      Distance.cosineDistance [FInt 1,FString "Joseph"] [FInt (-1),FString "Violet"] - 2.0 `shouldSatisfy` (< 0.0000001)
