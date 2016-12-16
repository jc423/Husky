import Test.Hspec
import Distance

main :: IO ();
main = hspec $ do
  describe "Distance" $ do
    it "euclidean distance should return euclidean distance between two vectors" $ do
      Distance.euclidean [1, 2] [4, 6] `shouldBe` 5

    it "manhattan distance should return manhattan distance between two vectors" $ do
      Distance.manhattan [0, 0] [2, 2] `shouldBe` 4
