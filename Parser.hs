module Parser (parseFile, Character(Character)) where
import qualified Data.ByteString as B
import System.IO
import Data.Binary.Get
import Data.Word
import Control.Monad
import Data.Char
import Data.Bits
import Codec.Binary.UTF8.String

type Name = String
data Character = Character Name Char

instance Show Character where
  show (Character name char) = show (name, char)

data CharRef = CharRef Char Int

instance Show CharRef where
  show (CharRef c i) = show (c, i)

parseFile :: FilePath -> IO [Character]
parseFile filename = withBinaryFile filename ReadMode $ \file -> do
  contents <- B.hGetContents file
  let charRefs = readCharacters contents
  return $ readRefNames contents charRefs

readCharacters :: B.ByteString -> [CharRef]
readCharacters file = map (extractChar file) [start,start+6..end]
  where start = extractInt file 4 4
        end   = extractInt file 8 4

extractChar :: B.ByteString -> Int -> CharRef
extractChar file offset =
  CharRef (chr $ extractInt file offset 2) (1 + (extractInt file (2+offset) 4))

extractInt :: B.ByteString -> Int -> Int -> Int
extractInt _ _ 0 = 0
extractInt file offset len = thisChar .|. restWord
  where thisChar = fromIntegral $ file `B.index` offset
        restWord = extractInt file (offset+1) (len-1) `shift` 8

readRefNames file refs = map (lookupCharName file) refs

lookupCharName file (CharRef c offset) =
  Character (extractString file offset) c

extractString :: B.ByteString -> Int -> String
extractString file offset = decode $ B.unpack $ B.takeWhile (not . zero) (B.drop offset file)
  where zero = (==) 0
