module FormatCommasSpec (spec) where

import Test.Hspec
import Lib
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "formatCommas" $ do

    it "formats zero" $ do
      formatCommas 0 `shouldBe` T.pack "0"

    it "formats single digit" $ do
      formatCommas 5 `shouldBe` T.pack "5"

    it "formats two digits" $ do
      formatCommas 42 `shouldBe` T.pack "42"

    it "formats three digits without commas" $ do
      formatCommas 999 `shouldBe` T.pack "999"

    it "formats four digits with one comma" $ do
      formatCommas 1000 `shouldBe` T.pack "1,000"

    it "formats one million" $ do
      formatCommas 1000000 `shouldBe` T.pack "1,000,000"

    it "formats large numbers" $ do
      formatCommas 1234567 `shouldBe` T.pack "1,234,567"

    it "formats positive trillion" $ do
      formatCommas 1234567890123 `shouldBe` T.pack "1,234,567,890,123"

    it "handles negative numbers" $ do
      formatCommas (-42) `shouldBe` T.pack "-42"

    it "handles negative thousand" $ do
      formatCommas (-1000) `shouldBe` T.pack "-1,000"

    it "formats negative large numbers with commas" $ do
      formatCommas (-1234567) `shouldBe` T.pack "-1,234,567"

    it "formats negative trillion" $ do
      formatCommas (-1234567890123) `shouldBe` T.pack "-1,234,567,890,123"
