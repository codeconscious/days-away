module Lib
    -- ( someFunc
    -- )
where

import Data.Time (diffDays, getCurrentTime, Day, UTCTime(utctDay))

data RowSummary = RowSummary {
      category :: String
    , summary  :: String
    , date     :: Day
    , daysAway :: Integer
}

instance Show RowSummary where
    show (RowSummary c s d da) = c ++ " | " ++ s ++ " | " ++ show d ++ " | " ++ show da
