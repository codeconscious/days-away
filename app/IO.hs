{-# OPTIONS_GHC -Wall -Werror #-} -- Treat warnings as errors.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-} -- Suppress such warnings.
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}

module IO (readSmallFile') where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Exception (IOException, try)
import Control.Monad (unless)
import Control.Monad.Except (liftEither, runExceptT, MonadError(throwError), ExceptT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Bifunctor (first)
import Data.Char (toLower)
import Data.Either (partitionEithers)
import Data.List (sortBy)
import Data.Text (Text)
import Data.Time (diffDays, getCurrentTime, Day, UTCTime(utctDay))
import Data.Ord (comparing)
import System.Environment (getArgs)
import System.FilePath (takeExtension)
import Text.Read (readEither)

readSmallFile' :: FilePath -> ExceptT String IO T.Text
readSmallFile' filePath = do
    result <- liftIO $ try @IOException (T.readFile filePath)
    liftEither $ first (("Error reading file: " ++) . show) result
