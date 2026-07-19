module Types (RowSummary(..), ColumnWidths(..), showWithColumns, computeColumnWidths, parseLine) where

import Lib (formatCommas)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (MonadError(throwError), ExceptT)
import Data.Bifunctor (Bifunctor(first))
import Data.Time (diffDays, Day)
import Text.Read (readEither)
import qualified Data.Text as T

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

parseLine :: Day -> String -> T.Text -> ExceptT String IO RowSummary
parseLine today separator line = do
    let parts = map T.strip $ T.splitOn (T.pack separator) line

    (c, s, d) <- case parts of
        [c, s, d] -> pure (c, s, d)
        _         -> throwError $ "* Error parsing malformed line: " <> T.unpack line

    parsedDate <- liftEither . first (formatDateError c s d) $ readEither (T.unpack d)

    return $ RowSummary c s parsedDate (diffDays today parsedDate)
  where
    formatDateError c s d err = unwords
        [ "* Error parsing date"
        , show (T.unpack d)
        , "in line with category"
        , show (T.unpack c)
        , "and summary"
        , show (T.unpack s) <> ":"
        , "`" <> err <> "`."
        ]

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

-- Gives the minimum column widths necessary to accommodate display of all row data, including padding spaces.
computeColumnWidths :: Int -> [RowSummary] -> ColumnWidths
computeColumnWidths padding summaries =
    ColumnWidths c s d da
      where
        findWidestInColumn finder = (+ padding) $ maximum $ finder <$> summaries
        c  = findWidestInColumn (T.length . category)
        s  = findWidestInColumn (T.length . summary)
        d  = findWidestInColumn (  length . show . date)
        da = findWidestInColumn (T.length . formatCommas . daysAway)
