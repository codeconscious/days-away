{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module IO (readSmallFile, printSummaries, printErrors) where

import Types (showWithColumns, ColumnWidths, RowSummary(..))
import Control.Exception (IOException, try)
import Control.Monad (unless)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Bifunctor (first)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.IO as T

readSmallFile :: FilePath -> ExceptT String IO T.Text
readSmallFile filePath = do
    result <- liftIO $ try @IOException (T.readFile filePath)
    liftEither $ first (("Error reading file: " ++) . show) result

printSummaries :: ColumnWidths -> [RowSummary] -> IO ()
printSummaries colWidths summaries = do
    mapM_ (putStrLn . showWithColumns colWidths) $ sortBy (comparing category) summaries

printErrors :: [String] -> IO ()
printErrors errs = do
    unless (null errs) $ do
        putStrLn $ show (length errs) ++ " parse error(s):"
        mapM_ putStrLn errs
