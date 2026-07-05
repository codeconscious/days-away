module Types (RowSummary(..), ColumnWidths(..), showWithColumns, renderRow) where

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

data ColumnWidths = ColumnWidths {
    categoryWidth :: Int
  , summaryWidth  :: Int
  , dateWidth     :: Int
  , daysAwayWidth :: Int
}

class RenderableRow a where
    renderRow :: ColumnWidths -> a -> String

-------------------

showWithColumns :: ColumnWidths -> RowSummary -> String
showWithColumns (ColumnWidths catLen sumLen dateLen daysLen)
                (RowSummary c s d da) =
    let filler = ' ' in
    T.unpack $ T.concat
        [ T.justifyLeft  catLen  filler c
        , T.justifyLeft  sumLen  filler s
        , T.justifyLeft  dateLen filler (T.pack $ show d)
        , T.justifyRight daysLen filler (formatCommas da)
        ]

instance RenderableRow RowSummary where
    renderRow cols row = showWithColumns cols row
