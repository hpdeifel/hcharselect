module HCharselect.Search where

import Data.Char
import Data.List

import HCharselect.Character

strMatcher str (Character n _ as) = any match (n:as)
  where match b = remBag str `isInfixOf` remBag b
        remBag = filter (not . flip elem ignore) . map toUpper
        ignore = " ,.\t'-_"
