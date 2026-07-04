module Types (RowSummary(..), ColumnLengths(..)) where

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
        T.unpack $ T.concat
            [ T.justifyLeft  20 filler c
            , T.justifyLeft  40 filler s
            , T.justifyLeft  12 filler (T.pack $ show d)
            , T.justifyRight 15 filler (formatCommas da)
            ]

categoryMax :: Int
categoryMax = 20

summaryMax :: Int
summaryMax = 40

dateMax :: Int
dateMax = 12

daysAwayMax :: Int
daysAwayMax = 15

data ColumnLengths = ColumnLengths {
    categoryLength :: Int
  , summaryLength  :: Int
  , dateLength     :: Int
  , daysAwayLength :: Int
}
