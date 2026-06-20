{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module IO (readSmallFile) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Exception (IOException, try)
import Control.Monad.Except (liftEither, ExceptT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Bifunctor (first)

readSmallFile :: FilePath -> ExceptT String IO T.Text
readSmallFile filePath = do
    result <- liftIO $ try @IOException (T.readFile filePath)
    liftEither $ first (("Error reading file: " ++) . show) result
