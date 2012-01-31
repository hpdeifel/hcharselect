-- | Contains just the definition of Character
module HCharselect.Character (
  Character(Character),
  Name,
  Alias
  ) where

import Control.DeepSeq

type Name = String
type Alias = String
data Character = Character Name Char [Alias] deriving Show

instance NFData Character where
  rnf (Character name char aliases) = name `deepseq` (char `deepseq` (aliases `deepseq` ()))
