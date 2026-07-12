module Types (RowSummary(..), ColumnWidths(..), showWithColumns, computeColumnWidths) where

import qualified Data.Text as T
import Data.Time (Day)
import Data.List (intercalate)

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

-- Format integer with comma-separated thousands.
formatCommas :: Integer -> T.Text
formatCommas n = T.pack $ sign ++ intercalate "," (reverse $ map reverse $ chunksOf 3 $ reverse $ show $ abs n)
  where
    sign = if n < 0 then "-" else ""
    chunksOf _ [] = []
    chunksOf n' xs = take n' xs : chunksOf n' (drop n' xs)

-- Returns the column widths necessary to display all summary text. Including padding spaces.
computeColumnWidths :: Int -> [RowSummary] -> ColumnWidths
computeColumnWidths padding summaries =
    ColumnWidths c s d da
      where
        findWidestInColumn finder = (+ padding) $ maximum $ finder <$> summaries
        c  = findWidestInColumn (T.length . category)
        s  = findWidestInColumn (T.length . summary)
        d  = findWidestInColumn (  length . show . date)
        da = findWidestInColumn (T.length . formatCommas . daysAway)
