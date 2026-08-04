module Lib (formatCommas, plur, PluralType(..)) where

import qualified Data.Text as T
import Data.List (intercalate)

-- | Format integer into comma-separated thousands.
formatCommas :: Integer -> T.Text
formatCommas n = T.pack $ sign <> intercalate "," (reverse $ map reverse $ chunksOf 3 $ reverse $ show $ abs n)
  where
    sign = if n < 0 then "-" else ""
    chunksOf _ [] = []
    chunksOf n' xs = take n' xs : chunksOf n' (drop n' xs)

-- | Specifies how a noun pluralizes.
data PluralType
    = S            -- ^ Add 's' suffix (e.g., "cat" → "cats")
    | Es           -- ^ Add 'es' suffix (e.g., "box" → "boxes")
    | Same         -- ^ Use same form for singular and plural (e.g., "sheep" → "sheep")
    | Irreg String -- ^ Irregular plural form provided as a string (e.g., "child" → "children")

-- | Format an integer with comma-separated thousands and pluralizes its noun as appropriate.
-- | Does not handle all-caps text. In that case, pass an Irreg term.
plur :: PluralType -> Integer -> String -> String
plur pt i text = unwords [T.unpack $ formatCommas i, plural pt i text]
  where
    plural _ n t | abs n == 1 = t
    plural S _ t = t <> "s"
    plural Es _ t = t <> "es"
    plural Same _ t = t
    plural (Irreg t') _ _ = t'
