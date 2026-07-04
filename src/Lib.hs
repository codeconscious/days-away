module Lib (formatCommas) where

import qualified Data.Text as T
import Data.List (intercalate)

-- Format integer with comma-separated thousands.
formatCommas :: Integer -> T.Text
formatCommas n = T.pack $ sign ++ intercalate "," (reverse groups)
  where
    sign = if n < 0 then "-" else ""
    chars = reverse . show $ abs n
    groups = chunksOf 3 chars
    chunksOf _ [] = []
    chunksOf k xs = take k xs : chunksOf k (drop k xs)
