module HCharselect.Search where

import Data.Char
import Data.List

import HCharselect.Character
import HCharselect.Utils

strOrCodeMatcher :: String -> Character -> Bool
strOrCodeMatcher str = case parseInt str of
  Just code -> codeMatcher code
  Nothing   -> strMatcher str

strMatcher :: String -> Character -> Bool
strMatcher str (Character n _ as) = any (match str) (n:as)

codeMatcher :: Int -> Character -> Bool
codeMatcher code (Character _ char _) = code == ord char

match needle haystack = prep needle `matchWords` prep haystack
  where prep = words . remBag
        remBag = filter (not . flip elem ignore) . map toUpper
        ignore = ",.'-_"

matchWords [] _ = True
matchWords _ [] = False
matchWords (n:ns) (h:hs)
  = if n `isInfixOf` h then matchWords ns hs else matchWords (n:ns) hs
