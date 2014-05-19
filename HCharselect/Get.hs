module HCharselect.Get
       ( Get 
       , runGet
       , offset 
       , current
       , goto
       , skip
       , count
       , sepBy
       , (<.|.>)
       , getWord8
       , getWord16
       , getWord32
       , getInt
       , getUTF16Char 
       , getNullTerminated
       , getUTF8String
       ) where

import qualified Data.ByteString as B
import Data.Word
import Data.Bits
import Data.Char
import Codec.Binary.UTF8.String
import Control.Applicative
import Control.Monad


-- A Get monad for binary parsing and randomly jumping around in the file

data S = S B.ByteString Int

newtype Get a = Get { unGet :: S -> (Either String a, S) }

instance Functor Get where
  f `fmap` g = Get (\s -> case unGet g s of
                       (Left e, s)   -> (Left e, s)
                       (Right a, s')   -> (Right (f a), s'))

instance Monad Get where
  return a = Get (\s -> (Right a, s))
  m >>= k  = Get (\s -> case unGet m s of
                     (Left a, s') -> (Left a, s')
                     (Right a, s') -> unGet (k a) s')
  fail err = Get (\s -> (Left err, s))

instance Applicative Get where
  pure = return
  (<*>)  = ap

offset :: Get Int
offset = Get (\(S b o) -> (Right o, S b o))

bytes :: Get B.ByteString
bytes = Get (\(S b o) -> (Right b, S b o))

current :: Get Word8
current = B.index <$> bytes <*> offset

goto :: Int -> Get ()
goto o = Get (\(S b _) -> (Right (), S b o))

skip :: Int -> Get ()
skip n = (+n) <$> offset >>= goto

count :: Int -> Get a -> Get [a]
count 0 _ = return []
count n g = (:) <$> g <*> count (n-1) g

sepBy :: (a -> Bool) -> Get a -> Get [a]
sepBy f g = do
  res <- g
  if f res
    then return []
    else (res:) <$> sepBy f g

(<.|.>) :: (Num c, Bits c, Integral a, Integral b) => Get a -> Get b -> Get c
a <.|.> b = do
  off1 <- offset
  ares <- getInt a
  off2 <- offset
  bres <- getInt b
  return $ (bres `shiftL` ((off2 - off1)*8)) .|. ares


getWord8 :: Get Word8
getWord8 = current <* skip 1

getWord16 :: Get Word16
getWord16 = getWord8 <.|.> getWord8

getWord32 :: Get Word32
getWord32 = getWord16 <.|.> getWord16

getInt :: (Integral a, Num b) => Get a -> Get b
getInt g = fromIntegral <$> g

getUTF16Char :: Get Char
getUTF16Char = chr <$> getInt getWord16

getNullTerminated :: Get [Word8]
getNullTerminated = sepBy (==0) getWord8

getUTF8String :: Get String
getUTF8String = decode <$> getNullTerminated



runGet :: Get a -> B.ByteString -> Either String a
runGet get bstring = fst $ unGet get (S bstring 0)
