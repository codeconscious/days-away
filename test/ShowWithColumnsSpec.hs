module ShowWithColumnsSpec (spec) where

import Test.Hspec
import Types
import Data.Time (fromGregorian)
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "showWithColumns" $ do
    let widths = ColumnWidths 10 15 12 10
        row c s da = RowSummary c s (fromGregorian 2026 7 20) da

    it "length of string is correct" $ do
      let result = showWithColumns widths (row (T.pack "Bug") (T.pack "Fix") 7)
      length result `shouldBe` 47

    it "formats with standard column widths" $ do
      let result = showWithColumns widths (row (T.pack "Work") (T.pack "Report") 0)
      length result `shouldBe` (10 + 15 + 12 + 10)

    it "left-justifies category field" $ do
      let result = showWithColumns widths (row (T.pack "Work") (T.pack "Report") 0)
      take 10 result `shouldBe` "Work      "

    it "left-justifies summary field" $ do
      let result = showWithColumns widths (row (T.pack "Work") (T.pack "Report") 0)
      take 15 (drop 10 result) `shouldBe` "Report         "

    it "left-justifies date field" $ do
      let result = showWithColumns widths (row (T.pack "Work") (T.pack "Report") 0)
      take 12 (drop 25 result) `shouldBe` "2026-07-20  "

    it "right-justifies daysAway field" $ do
      let result = showWithColumns widths (row (T.pack "Work") (T.pack "Report") 0)
      drop 37 result `shouldBe` "         0"

    it "handles negative daysAway" $ do
      let result = showWithColumns widths (row (T.pack "Work") (T.pack "Report") (-5))
      drop 37 result `shouldBe` "        -5"

    it "formats daysAway with commas for large numbers" $ do
      let result = showWithColumns widths (row (T.pack "Work") (T.pack "Report") 1234)
      drop 37 result `shouldBe` "     1,234"

    it "handles short text that gets padded" $ do
      let narrowWidths = ColumnWidths 5 5 5 5
          result = showWithColumns narrowWidths (row (T.pack "A") (T.pack "B") 1)
      result `shouldBe` "A    B    2026-07-20    1"

    it "handles text equal to column width" $ do
      let perfectWidths = ColumnWidths 4 6 10 1
          result = showWithColumns perfectWidths (row (T.pack "Work") (T.pack "Report") 1)
      result `shouldBe` "WorkReport2026-07-201"

    it "does not truncate category if text exceeds width" $ do
      let narrowWidths = ColumnWidths 3 15 12 10
          result = showWithColumns narrowWidths (row (T.pack "WorkCategory") (T.pack "Report") 0)
      take 4 result `shouldBe` "Work"

    it "does not truncate summary if text exceeds width" $ do
      let narrowWidths = ColumnWidths 10 5 12 10
          result = showWithColumns narrowWidths (row (T.pack "Work") (T.pack "ReportSummary") 0)
      take 13 (drop 10 result) `shouldBe` "ReportSummary"

    it "zero daysAway is right-justified" $ do
      let result = showWithColumns widths (row (T.pack "Task") (T.pack "Summary") 0)
      drop 37 result `shouldBe` "         0"

    it "large negative daysAway with commas" $ do
      let result = showWithColumns widths (row (T.pack "Work") (T.pack "Report") (-12345))
      drop 37 result `shouldBe` "   -12,345"
