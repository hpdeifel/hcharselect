module HCharselect.Search where

import Data.Char
import Data.List

import HCharselect.Character

strMatcher str (Character n _ as) = any (match str) (n:as)

match needle haystack = prep needle `matchWords` prep haystack
  where prep = words . remBag
        remBag = filter (not . flip elem ignore) . map toUpper
        ignore = ",.'-_"

matchWords [] _ = True
matchWords _ [] = False
matchWords (n:ns) (h:hs)
  = if n `isInfixOf` h then matchWords ns hs else matchWords (n:ns) hs
