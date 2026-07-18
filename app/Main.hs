{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}

module Main (main) where

import IO (readSmallFile, printSummaries, printErrors)
import Types (computeColumnWidths, parseLine)
import Validation (validateArgs, validateContent, validateExtension, validateLines)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (runExceptT, ExceptT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Either (partitionEithers)
import Data.Time (getCurrentTime, UTCTime(utctDay))
import qualified Data.Text as T

columnPaddingSpaces :: Int
columnPaddingSpaces = 3

csvSeparator :: String
csvSeparator = ","

main :: IO ()
main =
    runExceptT computation >>= either putStrLn return
    where
        computation :: ExceptT String IO () = do
            content <- validateArgs
                       >>= liftEither . validateExtension
                       >>= readSmallFile
                       >>= liftEither . validateContent
            let lines_ = dropInvalidLines $ T.lines content
            liftEither $ validateLines lines_
            today <- liftIO $ utctDay <$> getCurrentTime
            let lineCount = show $ length lines_
                charCount = show $ T.length content
                results   = mapM (runExceptT . parseLine today csvSeparator) lines_
            liftIO $ do
                putStrLn $ "This file has " ++ charCount ++ " total character(s) and " ++ lineCount ++ " data line(s)."
                (errors, summaries) <- partitionEithers <$> results
                let columnWidths = computeColumnWidths columnPaddingSpaces summaries
                printSummaries columnWidths summaries
                printErrors errors

dropInvalidLines :: [T.Text] -> [T.Text]
dropInvalidLines = filter isDataLine
  where
    commentMarker = '#'
    isDataLine line = case T.uncons line of
                      Just (hd, _) -> hd /= commentMarker
                      _            -> False

-- mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
-- mapWithIndex f xs = [f i x | (i, x) <- zip [0..] xs]
