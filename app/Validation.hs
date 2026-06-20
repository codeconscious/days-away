
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Validation (checkArgs, checkExtension) where

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

checkExtension :: FilePath -> ExceptT String IO ()
checkExtension path
    | isSupportedExt = return ()
    | otherwise      = throwError $ "Invalid file extension: " ++ ext
    where
        ext = map toLower $ takeExtension path
        isSupportedExt = ext == ".csv"
