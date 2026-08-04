module Lib.PlurSpec (spec) where

import Test.Hspec
import Lib

spec :: Spec
spec = do
  describe "plur" $ do

    it "leaves noun singular when count is 1" $ do
      let result = plur S 1 "cat"
      result `shouldBe` "1 cat"

    it "pluralizes with 's' suffix for PluralType S" $ do
      let result = plur S 5 "cat"
      result `shouldBe` "5 cats"

    it "pluralizes with 'es' suffix for PluralType Es" $ do
      let result = plur Es 2 "box"
      result `shouldBe` "2 boxes"

    it "uses same form for plural with PluralType Same" $ do
      let result = plur Same 10 "sheep"
      result `shouldBe` "10 sheep"

    it "uses irregular plural form for PluralType Irreg" $ do
      let result = plur (Irreg "children") 3 "child"
      result `shouldBe` "3 children"

    it "treats negative one as singular" $ do
      let result = plur S (-1) "apple"
      result `shouldBe` "-1 apple"

    it "pluralizes negative counts other than -1" $ do
      let result = plur S (-5) "banana"
      result `shouldBe` "-5 bananas"

    it "formats large numbers with comma separators" $ do
      let result = plur S 1000 "item"
      result `shouldBe` "1,000 items"

    it "formats very large numbers with comma separators" $ do
      let result = plur S 10000000 "item"
      result `shouldBe` "10,000,000 items"

    it "formats large negative numbers with comma separators" $ do
      let result = plur S (-1000) "item"
      result `shouldBe` "-1,000 items"

    it "formats very large negative numbers with comma separators" $ do
      let result = plur S (-10000000) "item"
      result `shouldBe` "-10,000,000 items"

    it "handles zero as plural" $ do
      let result = plur S 0 "thing"
      result `shouldBe` "0 things"
