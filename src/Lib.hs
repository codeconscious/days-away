module Lib (RowSummary(..), formatCommas) where

import Data.List (intercalate)
import Data.Time (Day)

data RowSummary = RowSummary {
      category :: String
    , summary  :: String
    , date     :: Day
    , daysAway :: Integer
}

instance Show RowSummary where
    show (RowSummary c s d da) = c ++ " | " ++ s ++ " | " ++ show d ++ " | " ++ formatCommas da

formatCommas :: Integer -> String
formatCommas n
  | n < 0 = '-' : formatCommas (abs n)
  | otherwise = intercalate "," . map reverse . reverse . chunksOf 3 . reverse $ show n
  where
    chunksOf _ [] = []
    chunksOf k xs = take k xs : chunksOf k (drop k xs)
