import Jwt.Tools
import Test.Hspec
import Prelude

main :: IO ()
main = hspec $ do
  describe "generateDpopToken" $ do
    it "should return a byte string" $ do
      testHaskellApi
      let actual = 1 :: Int
      let expected = 2 :: Int
      actual `shouldBe` expected
