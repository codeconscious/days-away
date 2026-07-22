{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Validation (validateArgs, validateArgsLogic, validateExtension, validateContent, validateLines, dropInvalidLines) where

import qualified Data.Text as T
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (MonadError(throwError), ExceptT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Char (toLower)
import System.Environment (getArgs)
import System.FilePath (takeExtension)

validateArgsLogic :: [String] -> Either String FilePath
validateArgsLogic args = case args of
    []    -> Left "You must provide the name of a properly-formatted CSV file as an argument."
    [arg] -> Right arg
    _     -> Left "Too many arguments! Provide only the name of a properly-formatted CSV file."

validateArgs :: ExceptT String IO FilePath
validateArgs = do
    args <- liftIO getArgs
    liftEither $ validateArgsLogic args

validateExtension :: FilePath -> ExceptT String IO FilePath
validateExtension path
    | isSupportedExt = return path
    | otherwise      = throwError $ "Invalid file extension: " <> ext
    where
        ext = map toLower $ takeExtension path
        isSupportedExt = ext == ".csv"

validateContent :: T.Text -> ExceptT String IO T.Text
validateContent text =
    case T.null text of
        True  -> throwError "The file was empty."
        False -> return text

validateLines :: [T.Text] -> ExceptT String IO [T.Text]
validateLines lines_ =
    case null lines_ of
        True  -> throwError "The file has text, but no data lines were found."
        False -> return lines_

dropInvalidLines :: [T.Text] -> [T.Text]
dropInvalidLines = filter isDataLine
  where
    commentMarker = '#'
    isDataLine line = case T.uncons line of
                      Just (hd, _) -> hd /= commentMarker
                      _            -> False
