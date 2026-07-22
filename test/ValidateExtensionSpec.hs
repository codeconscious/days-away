module ValidateExtensionSpec (spec) where

import Test.Hspec
import Validation
import Control.Monad.Except (runExceptT)

spec :: Spec
spec = do
  describe "validateExtension" $ do

    it "accepts .csv extension" $ do
      result <- runExceptT $ validateExtension "data.csv"
      result `shouldBe` Right "data.csv"

    it "accepts uppercase .CSV extension" $ do
      result <- runExceptT $ validateExtension "data.CSV"
      result `shouldBe` Right "data.CSV"

    it "accepts mixed case .Csv extension" $ do
      result <- runExceptT $ validateExtension "data.Csv"
      result `shouldBe` Right "data.Csv"

    it "accepts .cSv extension" $ do
      result <- runExceptT $ validateExtension "data.cSv"
      result `shouldBe` Right "data.cSv"

    it "rejects .txt extension" $ do
      result <- runExceptT $ validateExtension "data.txt"
      case result of
        Left err -> err `shouldContain` "Invalid file extension:"
        Right _ -> expectationFailure "Should reject .txt"

    it "rejects .json extension" $ do
      result <- runExceptT $ validateExtension "data.json"
      case result of
        Left err -> err `shouldContain` "Invalid file extension:"
        Right _ -> expectationFailure "Should reject .json"

    it "rejects .xlsx extension" $ do
      result <- runExceptT $ validateExtension "data.xlsx"
      case result of
        Left err -> err `shouldContain` "Invalid file extension:"
        Right _ -> expectationFailure "Should reject .xlsx"

    it "rejects extension of full-width characters" $ do
      result <- runExceptT $ validateExtension "file.ｃｓｖ"
      case result of
        Left err -> err `shouldContain` "Invalid file extension:"
        Right _ -> expectationFailure "Should reject .ｃｓｖ"

    it "rejects file with no extension" $ do
      result <- runExceptT $ validateExtension "data"
      case result of
        Left err -> err `shouldContain` "Invalid file extension:"
        Right _ -> expectationFailure "Should reject no extension"

    it "accepts .csv with directory path" $ do
      result <- runExceptT $ validateExtension "/home/user/documents/data.csv"
      result `shouldBe` Right "/home/user/documents/data.csv"

    it "accepts .csv with relative path" $ do
      result <- runExceptT $ validateExtension "../../data/data.csv"
      result `shouldBe` Right "../../data/data.csv"

    it "accepts .csv with dots in filename" $ do
      result <- runExceptT $ validateExtension "data.backup.2026.07.20.csv"
      result `shouldBe` Right "data.backup.2026.07.20.csv"

    it "rejects .csv with trailing text" $ do
      result <- runExceptT $ validateExtension "data.csv.backup"
      case result of
        Left err -> err `shouldContain` "Invalid file extension:"
        Right _ -> expectationFailure "Should reject .backup extension"

    it "error message includes extension" $ do
      result <- runExceptT $ validateExtension "data.txt"
      case result of
        Left err -> err `shouldContain` ".txt"
        Right _ -> expectationFailure "Should reject .txt"

    it "returns original path on success" $ do
      let testPath = "/path/to/my/file.csv"
      result <- runExceptT $ validateExtension testPath
      result `shouldBe` Right testPath

    it "handles Windows-style paths with .csv" $ do
      result <- runExceptT $ validateExtension "C:\\Users\\Documents\\data.csv"
      result `shouldBe` Right "C:\\Users\\Documents\\data.csv"

    it "handles empty filename with extension" $ do
      result <- runExceptT $ validateExtension ".csv"
      result `shouldBe` Right ".csv"
