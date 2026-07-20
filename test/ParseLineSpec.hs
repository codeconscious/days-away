module ParseLineSpec (spec) where

import Test.Hspec
import Types
import Data.Time (fromGregorian)
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "parseLine" $ do
    let today = fromGregorian 2026 7 20

    it "parses a valid line" $ do
      let line = T.pack "Work|Complete report|2026-07-20"
      case parseLine today "|" line of
        Right row -> do
          category row `shouldBe` T.pack "Work"
          summary row `shouldBe` T.pack "Complete report"
          daysAway row `shouldBe` 0
        Left _ -> expectationFailure "Should parse successfully"

    it "calculates daysAway for future date" $ do
      let line = T.pack "Work|Complete report|2026-08-20"
      case parseLine today "|" line of
        Right row -> daysAway row `shouldBe` (-31)
        Left _ -> expectationFailure "Should parse successfully"

    it "calculates daysAway for past date" $ do
      let line = T.pack "Work|Complete report|2026-07-10"
      case parseLine today "|" line of
        Right row -> daysAway row `shouldBe` 10
        Left _ -> expectationFailure "Should parse successfully"

    it "strips whitespace from all fields" $ do
      let line = T.pack "  Work  |  Complete report  |  2026-07-20  "
      case parseLine today "|" line of
        Right row -> do
          category row `shouldBe` T.pack "Work"
          summary row `shouldBe` T.pack "Complete report"
        Left _ -> expectationFailure "Should parse successfully"

    it "rejects line with too few fields" $ do
      let line = T.pack "Work|Complete report"
      case parseLine today "|" line of
        Left err -> err `shouldContain` "* Error parsing malformed line:"
        Right _ -> expectationFailure "Should fail to parse"

    it "rejects line with too many fields" $ do
      let line = T.pack "Work|Complete report|2026-07-20|Extra"
      case parseLine today "|" line of
        Left err -> err `shouldContain` "* Error parsing malformed line:"
        Right _ -> expectationFailure "Should fail to parse"

    it "rejects invalid date format" $ do
      let line = T.pack "Work|Complete report|invalid-date"
      case parseLine today "|" line of
        Left err -> err `shouldContain` "* Error parsing date"
        Right _ -> expectationFailure "Should fail to parse"

    it "handles different separator characters" $ do
      let line = T.pack "Work,Complete report,2026-07-20"
      case parseLine today "," line of
        Right row -> category row `shouldBe` T.pack "Work"
        Left _ -> expectationFailure "Should parse successfully"

    it "handles multi-character separator" $ do
      let line = T.pack "Work||Complete report||2026-07-20"
      case parseLine today "||" line of
        Right row -> category row `shouldBe` T.pack "Work"
        Left _ -> expectationFailure "Should parse successfully"
