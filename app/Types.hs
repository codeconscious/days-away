module Types (RowSummary(..)) where

import qualified Data.Text as T
import Lib (formatCommas)
import Data.Time (Day)

data RowSummary = RowSummary {
      category :: T.Text
    , summary  :: T.Text
    , date     :: Day
    , daysAway :: Integer
}

instance Show RowSummary where
    show (RowSummary c s d da) =
        let filler = ' ' in
        T.unpack (T.justifyLeft  20 filler c) ++
        T.unpack (T.justifyLeft  40 filler s) ++
        T.unpack (T.justifyLeft  12 filler (T.pack $ show d)) ++
        T.unpack (T.justifyRight 15 filler (formatCommas da))
