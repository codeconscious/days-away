{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Validation
    ( validateArgs
    , validateArgsLogic
    , validateExtension
    , validateContent
    , validateLines
    , dropInvalidLines
    , ValidatedFilePath(..)
    ) where

import qualified Data.Text as T
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (MonadError(throwError), ExceptT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Char (toLower)
import System.Environment (getArgs)
import System.FilePath (takeExtension)

newtype ValidatedFilePath = ValidatedFilePath FilePath
                            deriving (Show, Eq)

validateArgsLogic :: [String] -> Either String FilePath
validateArgsLogic []    = Left "You must provide the name of a properly-formatted CSV file as an argument."
validateArgsLogic [arg] = Right arg
validateArgsLogic _     = Left "Too many arguments! Provide only the name of a properly-formatted CSV file."

validateArgs :: ExceptT String IO FilePath
validateArgs = do
    args <- liftIO getArgs
    liftEither $ validateArgsLogic args

validateExtension :: FilePath -> ExceptT String IO ValidatedFilePath
validateExtension path
    | isSupportedExt = return $ ValidatedFilePath path
    | otherwise      = throwError $ "Invalid file extension: " <> extension
    where
        extension = takeExtension path
        isSupportedExt = map toLower extension == ".csv"

validateContent :: T.Text -> ExceptT String IO T.Text
validateContent text
    | T.null text = throwError "The file was empty."
    | otherwise   = return text

validateLines :: [T.Text] -> ExceptT String IO [T.Text]
validateLines lines'
    | null lines' = throwError "The file has text, but no data lines were found."
    | otherwise   = return lines'

dropInvalidLines :: [T.Text] -> [T.Text]
dropInvalidLines = filter isDataLine
  where
    commentMarker = '#'
    isDataLine line = not (T.null line) && T.head line /= commentMarker
