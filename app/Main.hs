module Main (main) where

import IO (readSmallFile, printSummaries, printErrors)
import Types (computeColumnWidths, parseLine)
import Validation (validateArgs, validateContent, validateExtension, validateLines, dropInvalidLines)
import Control.Monad.Except (runExceptT, ExceptT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Either (partitionEithers)
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
            content <- validateArgs
                       >>= validateExtension
                       >>= readSmallFile
                       >>= validateContent
            lines_ <- validateLines $ dropInvalidLines $ T.lines content
            let lineCount = show $ length lines_
                charCount = show $ T.length content
                (errors, summaries) = partitionEithers $ map (parseLine today separator) lines_
                columnWidths = computeColumnWidths columnPadding summaries
            liftIO $ do
                putStrLn $ "File has " <> charCount <> " total character(s) and " <> lineCount <> " data line(s)."
                printSummaries columnWidths summaries
                printErrors errors
