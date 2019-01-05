--module Tests where

import Text.Trifecta
import Test.Hspec
import Control.Exception (evaluate)
import Ex1
import Ex5

main :: IO ()
main = hspec $ do
  describe "according to Ord instance for SemVer" $ do
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
  describe "LogEntry parser" $ do
    it "aarses comments" $ do
      parseEither "-- foo" `shouldBe` Right (LogComment "foo")
    it "parses dates" $ do
      parseEither "# 2020-05-24" `shouldBe` Right (DateEntry 2020 5 24)
    it "parses dates ignoring comments" $ do
      parseEither "# 2020-05-24 -- foo" `shouldBe` Right (DateEntry 2020 5 24)  
    it "parses log entries" $ do
      parseEither "13:55 Foo bar" `shouldBe` Right (HourMinuteEntry 13 55 "Foo bar")
    it "parses log entries with special characters" $ do
      parseEither "13:55 Foo-bar" `shouldBe` Right (HourMinuteEntry 13 55 "Foo-bar")
    it "parses log entries ignoring comment" $ do
      parseEither "13:55 Foo-bar -- foo" `shouldBe` Right (HourMinuteEntry 13 55 "Foo-bar")
--    it "handles multiple lines" $ do
--      parseEither "-- foo\n\n# 2020-05-24\n13:55 Foo\n15:30 Bar\n" 

parseEither :: String -> Either String LogEntry
parseEither = resultAsEither . parse

resultAsEither :: Result a -> Either String a
resultAsEither (Success a) = Right a
resultAsEither (Failure e) = Left (show e)

parse :: String -> Result LogEntry
parse = parseString parseLine mempty

fakeLog :: String
fakeLog = "-- wheee a comment\n\n# 2025-02-05\n08:00 Breakfast\n09:00 Sanitizing moisture collector\n11:00 Exercising in high-grav gym\n12:00 Lunch\n13:00 Programming\n17:00 Commuting home in rover\n17:30 R&R\n19:00 Dinner\n21:00 Shower\n21:15 Read\n22:00 Sleep\n\n# 2025-02-07 -- dates not nececessarily sequential\n08:00 Breakfast -- should I try skippin bfast?\n09:00 Bumped head, passed out\n13:36 Wake up, headache\n13:37 Go to medbay\n13:40 Patch self up\n13:45 Commute home for rest\n14:15 Read\n21:00 Dinner\n21:15 Read\n22:00 Sleep\n"

checkLessThan :: String -> String -> Bool
checkLessThan f s =
  foldResult (\_ -> undefined) id ((<) <$> psv f <*> psv s)
