module Types (RowSummary(..)) where

import Lib(formatCommas)
import Data.Time (Day)

data RowSummary = RowSummary {
      category :: String
    , summary  :: String
    , date     :: Day
    , daysAway :: Integer
}

instance Show RowSummary where
    show (RowSummary c s d da) = c ++ " | " ++ s ++ " | " ++ show d ++ " | " ++ formatCommas da
