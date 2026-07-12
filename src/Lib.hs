module Lib
  ( formatCommas
  , checkArgs, validateExtension, validateContent, validateLines
  , RowSummary(..), ColumnWidths(..), showWithColumns, computeColumnWidths
  ) where

import Types (RowSummary(..), ColumnWidths(..), showWithColumns, computeColumnWidths)
import Validation (checkArgs, validateExtension, validateContent, validateLines)
import qualified Data.Text as T
import Data.List (intercalate)

-- Format integer into comma-separated thousands.
formatCommas :: Integer -> T.Text
formatCommas n = T.pack $ sign ++ intercalate "," (reverse $ map reverse $ chunksOf 3 $ reverse $ show $ abs n)
  where
    sign = if n < 0 then "-" else ""
    chunksOf _ [] = []
    chunksOf n' xs = take n' xs : chunksOf n' (drop n' xs)
