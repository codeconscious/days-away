module Types (RowSummary(..), ColumnWidths(..), showWithColumns, computeColumnWidths, parseLine) where

import Control.Monad.Except (MonadError(throwError), ExceptT)
import Data.List (intercalate)
import Data.Time (diffDays, Day)
import Text.Read (readEither)
import qualified Data.Text as T

data RowSummary = RowSummary {
      category :: T.Text
    , summary  :: T.Text
    , date     :: Day
    , daysAway :: Integer
}

parseLine :: Day -> String -> T.Text -> ExceptT String IO RowSummary
parseLine today separator line = do
    case T.splitOn (T.pack separator) line of
        [c, s, d] ->
            let (c', s', d') = (T.strip c, T.strip s, T.strip d) in
            case readEither $ T.unpack d' :: Either String Day of
                Left err -> throwError $ "* Error parsing date \"" ++ T.unpack d' ++ "\" in line \
                                         \with category " ++ show (T.unpack c') ++ " \
                                         \and summary " ++ show (T.unpack s') ++ ": `" ++ err ++ "`."
                Right parsedDate -> return $ RowSummary c' s' parsedDate (diffDays today parsedDate)
        _ -> throwError $ "* Error parsing malformed line: " ++ T.unpack line

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
