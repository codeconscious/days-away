module Types (RowSummary(..), ColumnWidths(..), showWithColumns) where

import qualified Data.Text as T
import Lib (formatCommas)
import Data.Time (Day)

data RowSummary = RowSummary {
      category :: T.Text
    , summary  :: T.Text
    , date     :: Day
    , daysAway :: Integer
}

data ColumnWidths = ColumnWidths {
    categoryWidth :: Int
  , summaryWidth  :: Int
  , dateWidth     :: Int
  , daysAwayWidth :: Int
}

showWithColumns :: ColumnWidths -> RowSummary -> String
showWithColumns (ColumnWidths cWidth sWidth dWidth daWidth)
                (RowSummary c s d da) =
    let filler = ' ' in
    T.unpack $ T.concat
        [ T.justifyLeft  cWidth  filler c
        , T.justifyLeft  sWidth  filler s
        , T.justifyLeft  dWidth  filler (T.pack $ show d)
        , T.justifyRight daWidth filler (formatCommas da)
        ]
