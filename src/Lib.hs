module Lib (formatCommas) where

import Data.List (intercalate)

formatCommas :: Integer -> String
formatCommas n
  | n < 0 = '-' : formatCommas (abs n)
  | otherwise = intercalate "," . map reverse . reverse . chunksOf 3 . reverse $ show n
  where
    chunksOf _ [] = []
    chunksOf k xs = take k xs : chunksOf k (drop k xs)
