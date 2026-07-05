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

columnPadding :: Int
columnPadding = 3

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
                results   = traverse (runExceptT . parseLine) lines_
            liftIO $ do
                putStrLn $ "This file has " ++ lineCount ++ " line(s) and " ++ charCount ++ " character(s)."
                (errors, summaries) <- partitionEithers <$> results
                let columnWidths = computeColumnWidths summaries
                printSummaries columnWidths summaries
                printErrors errors

ignoreInvalidLines :: [T.Text] -> [T.Text]
ignoreInvalidLines =
    filter (\line -> line /= T.empty && T.head line /= '#')

parseLine :: T.Text -> ExceptT String IO RowSummary
parseLine text = do
    now <- liftIO $ utctDay <$> getCurrentTime
    case T.splitOn separator text of
        [c, s, d] ->
            let dayParseResult = readEither $ T.unpack d :: Either String Day in
            case dayParseResult of
                Left err  -> throwError $ "* Error parsing date \"" ++ T.unpack d ++ "\" in line \
                                          \with category " ++ show (T.unpack c) ++ " \
                                          \and summary " ++ show (T.unpack s) ++ ": `" ++ err ++ "`."
                Right day -> return $ RowSummary c (T.strip s) day (diffDays now day)
        _ -> throwError $ "* Error parsing malformed line: " ++ T.unpack text
    where
        separator = T.pack ","

computeColumnWidths :: [RowSummary] -> ColumnWidths
computeColumnWidths summaries =
    ColumnWidths c s d da
      where
        findMax finder = (+ columnPadding) $ maximum $ fmap finder summaries
        c  = findMax (T.length . category)
        s  = findMax (T.length . summary)
        d  = findMax (  length . show . date)
        da = findMax (T.length . formatCommas . daysAway)

printSummaries :: ColumnWidths -> [RowSummary] -> IO ()
printSummaries colWidths summaries = do
    -- mapM_ print $ sortBy (comparing category) summaries
    mapM_ (putStrLn . showWithColumns colWidths) $ sortBy (comparing category) summaries

printErrors :: [String] -> IO ()
printErrors errs = do
    unless (null errs) $ do
        putStrLn $ "There were " ++ show (length errs) ++ " parse error(s):"
        mapM_ putStrLn errs

-- mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
-- mapWithIndex f xs = [f i x | (i, x) <- zip [0..] xs]
