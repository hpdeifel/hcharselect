module HCharselect.Parser (parseFile, Character(Character)) where
import qualified Data.ByteString as B
import System.IO
import Data.Word
import Control.Monad
import Data.Char
import Data.Bits
import Control.DeepSeq
import Control.Applicative
import Control.Parallel.Strategies
import Data.List

import HCharselect.Get

type Name = String
type Alias = String
data Character = Character Name Char [Alias] deriving Show

instance NFData Character where
  rnf (Character name char aliases) = name `deepseq` (char `deepseq` (aliases `deepseq` ()))

data CharRef = CharRef Char Int deriving (Show)

data Detail = Detail {
  detChar :: Char,
  alias_offset :: Int,
  alias_count :: Int
} deriving (Show)

data Header = Header {
  names_begin :: Int,
  names_end :: Int,
  details_begin :: Int,
  details_end :: Int
} deriving (Show)

instance NFData Header where
  rnf (Header nb ne db de) = nb `seq` ne `seq` db `seq` de `seq` ()

parseFile :: FilePath -> IO [Character]
parseFile filename = withBinaryFile filename ReadMode $ \file -> do
  contents <- B.hGetContents file
  case runGet parseFile' contents of
    Right a -> return a
    Left err -> fail err

with :: Get a -> Strategy a -> Get a
with g s = do { r <- g; return (r `using` s); }


parseFile' :: Get [Character]
parseFile' = do
  header <- parseHeader `with` rdeepseq
  charRefs <- parseCharRefs header
  details <- parseDetails header

  refs <- readRefNames charRefs `with` (parList rdeepseq)
  aliases <- readRefAliases details `with` (parList rdeepseq)

  return $ (merge refs aliases `using` rdeepseq)

parseHeader :: Get Header
parseHeader = goto 4 *> (Header <$> int <*> int <*> int <*> int)
  where int = getInt getWord32

parseCharRefs :: Header -> Get [CharRef]
parseCharRefs header = goto start *> count ((end-start)`quot`6) readCharRef
  where start = names_begin header
        end   = names_end header

parseDetails :: Header -> Get [Detail]
parseDetails header = goto start *> count ((end-start)`quot`27) readDetail
  where start = details_begin header
        end   = details_end header
 
readCharRef :: Get CharRef
readCharRef = CharRef <$> getUTF16Char <*> ((+1) <$> getInt getWord32)

readRefNames :: [CharRef] -> Get [(Char, Name)]
readRefNames refs = mapM lookupCharName refs

lookupCharName :: CharRef -> Get (Char, Name)
lookupCharName (CharRef c offset) = goto offset *> do
  str <- getUTF8String
  return (c, str)

readDetail :: Get Detail
readDetail = Detail <$> getUTF16Char <*> getInt getWord32 <*> getInt getWord8 <* skip 20

readRefAliases :: [Detail] -> Get [(Char, [Alias])]
readRefAliases dets = mapM lookupDetails dets

lookupDetails :: Detail -> Get (Char, [Alias])
lookupDetails (Detail c off cnt) = goto off *> do
  strs <- count cnt getUTF8String
  return (c, strs)

merge :: [(Char, Name)] -> [(Char, [Alias])] -> [Character]
merge ns ds = helper (sortBy sorter ns) (sortBy sorter ds)
  where sorter x y = compare (fst x) (fst y)
        helper [] _ = []
        helper ns [] = map (\(c,n) -> Character n c []) ns
        helper ns@((c1,n):ns') ds@((c2,as):ds')
          | c1 < c2   = (Character n c1 []):(helper ns' ds)
          | c1 == c2  = (Character n c1 as):(helper ns' ds')
          | otherwise = helper ns ds'
