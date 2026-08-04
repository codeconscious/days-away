module Main (main) where

import IO (readSmallFile, printSummaries, printErrors)
import Lib (plurS)
import CsvData (computeColumnWidths, parseLine)
import Validation (validateArgs, validateContent, validateExtension, validateLines, dropInvalidLines)
import Control.Monad.Except (runExceptT, ExceptT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Either (partitionEithers)
import Data.List (intercalate)
import Data.Time (getCurrentTime, UTCTime(utctDay))
import qualified Data.Text as T

columnPadding :: Int
columnPadding = 3

separator :: String
separator = ","

main :: IO ()
main =
    runExceptT computation >>= either putStrLn return
      where
        computation :: ExceptT String IO () = do
            today <- liftIO $ utctDay <$> getCurrentTime
            content <- validateArgs >>= validateExtension >>= readSmallFile >>= validateContent
            lines' <- validateLines $ dropInvalidLines $ T.lines content
            let charCount = plurS (toInteger $ T.length content) "character"
                lineCount = plurS (toInteger $ length lines') "data line"
                (errors, summaries) = partitionEithers $ parseLine today separator <$> lines'
                columnWidths = computeColumnWidths columnPadding summaries
            liftIO $ do
                putStrLn $ "File summary: " <> intercalate ", " [charCount, lineCount] <> "."
                printSummaries columnWidths summaries
                printErrors errors
