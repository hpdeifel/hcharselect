module HCharselect.Utils where

parseInt :: String -> Maybe Int
parseInt str = case reads str :: [(Int, String)] of
  [(code,rest)] -> if rest == "" then Just code else Nothing
  []            -> Nothing

