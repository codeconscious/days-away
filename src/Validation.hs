{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Validation (validateArgs, validateExtension, validateContent, validateLines) where

import qualified Data.Text as T
import Control.Monad.Except (MonadError(throwError), ExceptT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Char (toLower)
import System.Environment (getArgs)
import System.FilePath (takeExtension)

validateArgs :: ExceptT String IO FilePath
validateArgs = do
    args <- liftIO getArgs
    case args of
        []    -> throwError "You must provide the name of a properly-formatted CSV file as an argument."
        [arg] -> return arg
        _     -> throwError "Too many arguments! Provide only the name of a properly-formatted CSV file."

validateExtension :: FilePath -> Either String FilePath
validateExtension path
    | isSupportedExt = return path
    | otherwise      = throwError $ "Invalid file extension: " ++ ext
    where
        ext = map toLower $ takeExtension path
        isSupportedExt = ext == ".csv"

validateContent :: T.Text -> Either String T.Text
validateContent text =
    case T.null text of
        True  -> throwError "The file was empty."
        False -> return text

validateLines :: [T.Text] -> Either String ()
validateLines lines_ =
    case null lines_ of
        True  -> throwError "The file has text, but no data lines were found."
        False -> return ()
