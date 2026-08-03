module Lib (formatCommas, plurS) where

import qualified Data.Text as T
import Data.List (intercalate)

-- |Format integer into comma-separated thousands.
formatCommas :: Integer -> T.Text
formatCommas n = T.pack $ sign <> intercalate "," (reverse $ map reverse $ chunksOf 3 $ reverse $ show $ abs n)
  where
    sign = if n < 0 then "-" else ""
    chunksOf _ [] = []
    chunksOf n' xs = take n' xs : chunksOf n' (drop n' xs)

-- | Format an integer with comma-separated thousands and pluralize a noun with `s`.
plurS :: Integer -> String -> String
plurS i text = unwords [T.unpack $ formatCommas i, plural i text]
  where
    plural 1 t = t
    plural _ t = t <> "s"
