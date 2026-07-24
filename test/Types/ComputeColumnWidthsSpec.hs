module Types.ComputeColumnWidthsSpec (spec) where

import Test.Hspec
import Types
import qualified Data.Text as T
import Data.Time (fromGregorian)

spec :: Spec
spec = do
  describe "computeColumnWidths" $ do

    it "computes correct widths for a single row with padding" $ do
      let summaries = [RowSummary (T.pack "cat") (T.pack "summ") (fromGregorian 2026 7 8) 5]
          result = computeColumnWidths 2 summaries
      categoryWidth result `shouldBe` 5   -- "cat" (3) + padding (2)
      summaryWidth result `shouldBe` 6    -- "summ" (4) + padding (2)
      dateWidth result `shouldBe` 12      -- show of date is ~10 chars + padding (2)
      daysAwayWidth result `shouldBe` 3   -- "5" (1) + padding (2)

    it "applies padding to all columns" $ do
      let summaries = [RowSummary (T.pack "a") (T.pack "b") (fromGregorian 2026 1 1) 1]
          result = computeColumnWidths 5 summaries
      let minExpected = 6  -- shortest field "a" or "b" (1) + padding (5)
      categoryWidth result `shouldBe` minExpected
      summaryWidth result `shouldBe` minExpected

    it "selects widest column when multiple rows present" $ do
      let summaries =
            [ RowSummary (T.pack "short") (T.pack "a") (fromGregorian 2026 1 1) 1
            , RowSummary (T.pack "verylongcategory") (T.pack "b") (fromGregorian 2026 1 1) 1
            ]
          result = computeColumnWidths 1 summaries
      categoryWidth result `shouldBe` 17  -- "verylongcategory" (16) + padding (1)
      summaryWidth result `shouldBe` 2    -- "a" (1) + padding (1)

    it "handles large daysAway values with comma formatting" $ do
      let summaries = [RowSummary (T.pack "x") (T.pack "y") (fromGregorian 2026 1 1) 1000000]
          result = computeColumnWidths 0 summaries
      daysAwayWidth result `shouldBe` 9 -- "1,000,000" is 9 characters

    it "handles negative daysAway values" $ do
      let summaries = [RowSummary (T.pack "x") (T.pack "y") (fromGregorian 2026 1 1) (-100)]
          result = computeColumnWidths 1 summaries
      daysAwayWidth result `shouldBe` 5 -- "-100" is 4 characters + padding (1)

    it "handles zero padding" $ do
      let summaries = [RowSummary (T.pack "test") (T.pack "data") (fromGregorian 2026 5 15) 42]
          result = computeColumnWidths 0 summaries
      categoryWidth result `shouldBe` 4
      summaryWidth result `shouldBe` 4

    it "handles unicode text correctly" $ do
      let summaries = [RowSummary (T.pack "café") (T.pack "naïve") (fromGregorian 2026 1 1) 0]
          result = computeColumnWidths 1 summaries
      -- T.length counts characters, not bytes
      categoryWidth result `shouldBe` 5   -- "café" (4) + padding (1)
      summaryWidth result `shouldBe` 6    -- "naïve" (5) + padding (1)

    it "returns all four column widths" $ do
      let summaries = [RowSummary (T.pack "a") (T.pack "b") (fromGregorian 2026 1 1) 0]
          result = computeColumnWidths 1 summaries
      -- Verify all fields are present and positive
      categoryWidth result `shouldSatisfy` (> 0)
      summaryWidth result `shouldSatisfy` (> 0)
      dateWidth result `shouldSatisfy` (> 0)
      daysAwayWidth result `shouldSatisfy` (> 0)
