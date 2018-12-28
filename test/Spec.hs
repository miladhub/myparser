module Tests where

import Text.Trifecta
import Test.Hspec
import Control.Exception (evaluate)
import Ex1

main :: IO ()
main = hspec $ do
  describe "Ord instance for SemVer says:" $ do
    it "SemVer 2 1 0 [] [] < SemVer 2 1 1 [] []" $ do
      (SemVer 2 1 0 [] [] < SemVer 2 1 1 [] []) `shouldBe` True
    it "1.0.0-alpha < 1.0.0-alpha.1" $ do
      checkLessThan "1.0.0-alpha" "1.0.0-alpha.1" `shouldBe` True
    it "1.0.0-alpha.1 < 1.0.0-alpha.beta" $ do
      checkLessThan "1.0.0-alpha.1" "1.0.0-alpha.beta" `shouldBe` True
    it "1.0.0-alpha.beta < 1.0.0-beta.2" $ do
      checkLessThan "1.0.0-alpha.beta" "1.0.0-beta.2" `shouldBe` True
    it "1.0.0-beta.2 < 1.0.0-beta.11" $ do
      checkLessThan "1.0.0-beta.2" "1.0.0-beta.11" `shouldBe` True
    it "1.0.0-beta.11 < 1.0.0-rc.1" $ do
      checkLessThan "1.0.0-beta.11" "1.0.0-rc.1" `shouldBe` True
    it "1.0.0-rc.1 < 1.0.0" $ do
      checkLessThan "1.0.0-rc.1" "1.0.0" `shouldBe` True

checkLessThan :: String -> String -> Bool
checkLessThan f s =
  foldResult (\_ -> undefined) id ((<) <$> psv f <*> psv s)
