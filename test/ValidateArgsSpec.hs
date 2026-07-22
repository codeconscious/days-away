{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module ValidateArgsSpec (spec) where

import Test.Hspec
import Validation

spec :: Spec
spec = do
  describe "validateArgsLogic" $ do

    it "accepts single argument" $ do
      validateArgsLogic ["data.csv"] `shouldBe` Right "data.csv"

    it "accepts single argument with path" $ do
      validateArgsLogic ["/home/user/data.csv"] `shouldBe` Right "/home/user/data.csv"

    it "rejects no arguments" $ do
      let result = validateArgsLogic []
      case result of
        Left err -> err `shouldContain` "You must provide the name of a properly-formatted CSV file as an argument."
        Right _ -> expectationFailure "Should reject empty args"

    it "rejects two arguments" $ do
      let result = validateArgsLogic ["file1.csv", "file2.csv"]
      case result of
        Left err -> err `shouldContain` "Too many arguments!"
        Right _ -> expectationFailure "Should reject two arguments"

    it "rejects three arguments" $ do
      let result = validateArgsLogic ["file1.csv", "file2.csv", "file3.csv"]
      case result of
        Left err -> err `shouldContain` "Too many arguments!"
        Right _ -> expectationFailure "Should reject three arguments"

    it "preserves argument exactly" $ do
      validateArgsLogic ["MyFile.CSV"] `shouldBe` Right "MyFile.CSV"

    it "handles argument with spaces" $ do
      validateArgsLogic ["my file.csv"] `shouldBe` Right "my file.csv"

    it "handles argument with special characters" $ do
      validateArgsLogic ["data-2026_07_20.csv"] `shouldBe` Right "data-2026_07_20.csv"

    it "handles argument with Japanese characters" $ do
      validateArgsLogic ["私の素敵なファイル.csv"] `shouldBe` Right "私の素敵なファイル.csv"

    it "error message for no args is precise" $ do
      let Left err = validateArgsLogic []
      err `shouldBe` "You must provide the name of a properly-formatted CSV file as an argument."

    it "error message for too many args is precise" $ do
      let Left err = validateArgsLogic ["a", "b"]
      err `shouldBe` "Too many arguments! Provide only the name of a properly-formatted CSV file."
