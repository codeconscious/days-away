module Lib (formatCommas) where

import Data.List (intercalate)
import qualified Data.Text as T

formatCommas :: Integer -> T.Text
formatCommas n
  | n < 0 = T.append (T.pack "-") (formatCommas (abs n))
  | otherwise = T.pack $ intercalate "," . map reverse . reverse . chunksOf 3 . reverse $ show n
  where
    chunksOf _ [] = []
    chunksOf k xs = take k xs : chunksOf k (drop k xs)
