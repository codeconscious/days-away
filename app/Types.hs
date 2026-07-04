module Types (RowSummary(..)) where

import qualified Data.Text as T
import Lib(formatCommas)
import Data.Time (Day)

data RowSummary = RowSummary {
      category :: T.Text
    , summary  :: T.Text
    , date     :: Day
    , daysAway :: Integer
}

instance Show RowSummary where
    show (RowSummary c s d da) =
        T.unpack (T.justifyLeft 20 ' ' c) ++
        T.unpack (T.justifyLeft 40 ' ' s) ++
        show d ++
        T.unpack (T.justifyRight 15 ' ' (formatCommas da))
