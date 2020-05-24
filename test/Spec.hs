import Test.Hspec
import Lib

main :: IO()
main = hspec $ do
   describe "Tests de prueba" $ do
      it "la verdad y la verdad" $ do
         True `shouldBe` True
