module HCharselect.Cli where

import Control.Monad

import HCharselect.Parser
import HCharselect.Search


cli :: [Character] -> String -> IO ()
cli chars search = mapM_ printChar filtered
  where filtered = filter (strMatcher search) chars

printChar :: Character -> IO ()
printChar (Character n c _) = putStrLn (c:'\t':n)
