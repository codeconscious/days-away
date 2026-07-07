{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}

module Main (main) where

import Types --
import IO (readSmallFile)
import Validation (checkArgs, validateExtension, validateContent, validateLines)
import qualified Data.Text as T
import Control.Monad (unless)
import Control.Monad.Except (runExceptT, MonadError(throwError), ExceptT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Either (partitionEithers)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Time (diffDays, getCurrentTime, Day, UTCTime(utctDay))
import Text.Read (readEither)
import Control.Monad.Error.Class (liftEither)
import Data.Function ((&))
import Lib (formatCommas)

columnPaddingSpaces :: Int
columnPaddingSpaces = 3

csvSeparator :: String
csvSeparator = ","

main :: IO ()
main =
    runExceptT computation >>= either putStrLn return
    where
        computation :: ExceptT String IO () = do
            fileName <- checkArgs
            validateExtension fileName & liftEither
            content <- readSmallFile fileName
            validateContent content & liftEither
            let lines_ = T.lines content & ignoreInvalidLines
            validateLines lines_ & liftEither
            let lineCount = show $ length lines_
                charCount = show $ T.length content
                results   = traverse (runExceptT . parseLine csvSeparator) lines_
            liftIO $ do
                putStrLn $ "This file has " ++ lineCount ++ " data line(s) and " ++ charCount ++ " character(s)."
                (errors, summaries) <- partitionEithers <$> results
                let columnWidths = computeColumnWidths columnPaddingSpaces summaries
                printSummaries columnWidths summaries
                printErrors errors

ignoreInvalidLines :: [T.Text] -> [T.Text]
ignoreInvalidLines = filter (\line -> line /= T.empty && T.head line /= '#')

parseLine :: String -> T.Text -> ExceptT String IO RowSummary
parseLine separator text = do
    now <- liftIO $ utctDay <$> getCurrentTime
    case T.splitOn (T.pack separator) text of
        [c, s, d] ->
            let dayParseResult = readEither $ T.unpack (T.strip d) :: Either String Day in
            case dayParseResult of
                Left err  -> throwError $ "* Error parsing date \"" ++ T.unpack (T.strip d) ++ "\" in line \
                                          \with category " ++ show (T.unpack $ T.strip c) ++ " \
                                          \and summary " ++ show (T.unpack $ T.strip s) ++ ": `" ++ err ++ "`."
                Right day -> return $ RowSummary (T.strip c) (T.strip s) day (diffDays now day)
        _ -> throwError $ "* Error parsing malformed line: " ++ T.unpack text

-- Returns the column widths necessary to display all summary text. Including padding spaces.
computeColumnWidths :: Int -> [RowSummary] -> ColumnWidths
computeColumnWidths padding summaries =
    ColumnWidths c s d da
      where
        findWidestInColumn finder = (+ padding) $ maximum $ finder <$> summaries
        c  = findWidestInColumn (T.length . category)
        s  = findWidestInColumn (T.length . summary)
        d  = findWidestInColumn (  length . show . date)
        da = findWidestInColumn (T.length . formatCommas . daysAway)

printSummaries :: ColumnWidths -> [RowSummary] -> IO ()
printSummaries colWidths summaries = do
    mapM_ (putStrLn . showWithColumns colWidths) $ sortBy (comparing category) summaries

printErrors :: [String] -> IO ()
printErrors errs = do
    unless (null errs) $ do
        putStrLn $ "There were " ++ show (length errs) ++ " parse error(s):"
        mapM_ putStrLn errs

-- mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
-- mapWithIndex f xs = [f i x | (i, x) <- zip [0..] xs]
