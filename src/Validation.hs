{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Validation (checkArgs, validateExtension, validateContent, validateLines) where

import qualified Data.Text as T
import Control.Monad.Except (MonadError(throwError), ExceptT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Char (toLower)
import System.Environment (getArgs)
import System.FilePath (takeExtension)

checkArgs :: ExceptT String IO FilePath
checkArgs = do
    args <- liftIO getArgs
    case args of
        []    -> throwError "You must provide the name of a CSV as an argument."
        [arg] -> return arg
        _     -> throwError "Too many arguments! Provide only the name of a CSV containing dates."

validateExtension :: FilePath -> Either String ()
validateExtension path
    | isSupportedExt = return ()
    | otherwise      = throwError $ "Invalid file extension: " ++ ext
    where
        ext = map toLower $ takeExtension path
        isSupportedExt = ext == ".csv"

validateContent :: T.Text -> Either String ()
validateContent text =
    case T.null text of
        True  -> throwError "The file was empty."
        False -> return ()

validateLines :: [T.Text] -> Either String ()
validateLines lines_ =
    case null lines_ of
        True  -> throwError "The file has text, but no data lines were found."
        False -> return ()
