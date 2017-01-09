module Supervised.KNNSpec (main, spec) where

import Test.Hspec
import HuskyML
import Supervised.KNN
import Distance

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  -- when choosing unknown we try to make visible match with cluster
  describe "KNN" $ do
    describe "Classification" $ do
      describe "euclidean distance" $ do
        it "with only one train data and one neighbor should return that label" $ do
          euclideanKNNClassification 1 weighted [Classified [FInt 3] "Joe"] [FInt 2] `shouldBe` "Joe"

        it "with only one train data and multiple neighbors should return that label" $ do
          euclideanKNNClassification 3 weighted [Classified [FInt 3] "Joe"] [FInt 2] `shouldBe` "Joe"

        describe "weighted" $ do
          it "with many train data and one neighbor should return that label of closest neighbor" $ do
            euclideanKNNClassification 1 weighted carsData [FInt 2015, FString "Subaru", FInt 141] `shouldBe` "nice"

          it "with many train data and multiple neighbors should return expected label" $ do
            euclideanKNNClassification 3 weighted carsData [FInt 2015, FString "Subaru", FInt 141] `shouldBe` "nice"

        describe "mostCommon" $ do
          it "with many train data and one neighbor should return that label of closest neighbor" $ do
            euclideanKNNClassification 1 mostCommon carsData [FInt 2015, FString "Subaru", FInt 141] `shouldBe` "nice"

          it "with many train data and multiple neighbors should return expected label" $ do
            euclideanKNNClassification 3 mostCommon carsData [FInt 2015, FString "Subaru", FInt 141] `shouldBe` "eh"

      -- custom distance function inverts euclidean distance
      describe "custom distance" $ do
        describe "weighted" $ do
          it "with many train data and one neighbor should return that closest neighbor" $ do
            knnClassification customDistance 1 weighted carsData [FInt 2015, FString "Subaru", FInt 141] `shouldBe` "crappy"

          it "with many train data and multiple neighbors should return expected label" $ do
            knnClassification customDistance 1 weighted carsData [FInt 2015, FString "Subaru", FInt 141] `shouldBe` "crappy"

        describe "mostCommon" $ do
          it "with many train data and one neighbor should return that label of closest neighbor" $ do
            knnClassification customDistance 1 mostCommon carsData [FInt 2015, FString "Subaru", FInt 141] `shouldBe` "crappy"

          it "with many train data and multiple neighbors should return expected label" $ do
            knnClassification customDistance 3 mostCommon carsData [FInt 2015, FString "Subaru", FInt 141] `shouldBe` "reliable"

      describe "Regression" $ do
        it "with only one training data and one neighbor should return value of training data" $ do
          euclideanKNNRegression 1 [Classified [FInt 3] 4.0] [FInt 2] `shouldBe` 4.0

        it "with multiple training data and multiple neighbors should return the average of the closest neighbors" $ do
          euclideanKNNRegression 2 [Classified [FInt 3] 4.0, Classified [FInt 6] 10.0, Classified [FInt 4] 6.0] [FInt 2] `shouldBe` 5.0 -- should average 1st and 3rd training data


  describe "KDTree" $ do
    it "should return a binary tree of two levels given list of classifieds and list of one splits" $ do
      let a = Classified [FInt 4] 6.0
          b = Classified [FInt 2] 6.0
          c = Classified [FInt 3] 4.0
          d = Classified [FInt 6] 10.0
        in createKDTree [a, b, c, d] [0] `shouldBe` Node {this=a, left=Leaf [b,c], right=Leaf [d]}

    it "should return a binary tree of two levels given list of three classifieds and list of two splits" $ do
      let a = Classified [FInt 4] 6.0
          b = Classified [FInt 2] 6.0
          c = Classified [FInt 3] 4.0
        in createKDTree [a, b, c] [0,1] `shouldBe` Node {this=c, left=Node {this=b, left=Leaf [], right=Leaf []}, right=Node {this=a, left=Leaf [], right=Leaf []}}


-- this distance function will actually result in labels that are farthest away
customDistance xs ys = 1 / euclidean xs ys

-- attempted to make 3 clusters
-- Cluster 1 containing crappy, 2 reliable
-- Cluster 2 containing wow
-- Cluster 3 containing nice, 2 wow
carsData = [
  Classified { features=[FInt 1979, FString "Toyota", FInt 1500], label="crappy" }, -- FInt 1979 impacts weighted KNN
  Classified { features=[FInt 1983, FString "Toyota", FInt 1500], label="reliable" },
  Classified { features=[FInt 1983, FString "Nissan", FInt 1500], label="reliable" },
  Classified { features=[FInt 2000, FString "Toyota", FInt 1100], label="wow" },
  Classified { features=[FInt 2010, FString "Subaru", FInt 140], label="eh" },-- FInt 2010 impacts weighted KNN
  Classified { features=[FInt 2014, FString "Subaru", FInt 140], label="eh" },
  Classified { features=[FInt 2015, FString "Subaru", FInt 140], label="nice" }];


