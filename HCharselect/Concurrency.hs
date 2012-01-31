{-# LANGUAGE DeriveDataTypeable #-}
module HCharselect.Concurrency 
       ( parseThread
       , filterThread
       , parSearch
       , newCompCtx
       , runIn
       , execCtx
       ) where

import Control.Concurrent
import Control.Parallel.Strategies
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Exception.Base
import Control.Monad
import Data.Typeable
import Data.List
import Data.IORef

import HCharselect.Parser
import HCharselect.Search

parseParallel :: FilePath -> IO [Character]
parseParallel filename = do
  start <- getCurrentTime
  chars <- parseFile filename
  end <- getCurrentTime
  putStrLn $ "Parsing took " ++ show (end `diffUTCTime` start)
  return chars

parseThread var file = do
  chars <- fmap (flip using rdeepseq) $ parseParallel file
  putMVar var chars


-- | A list of computations that are to be executed in another thread.
-- The idea is that one thread adds computations and another runs them
-- when it it idle.
-- This is particularly suited for Gtk, because we have to run all the
-- drawing operations in the main thread.
newtype ComputationContext = CompCtx (MVar [IO ()])

-- | Create a new computation context
newCompCtx :: IO ComputationContext
newCompCtx = fmap CompCtx $ newMVar []

-- | Queue an IO action for execution
runIn :: ComputationContext -> IO () -> IO ()
runIn (CompCtx var) action = modifyMVar_ var $ return . (action:)

-- | Execute all actions in the current thread
execCtx :: ComputationContext -> IO ()
execCtx (CompCtx var) = swapMVar var [] >>= execList
  where execList [] = return ()
        execList (a:as) = a >> execList as
  

data StopComputing = StopComputing deriving (Show, Typeable)
instance Exception StopComputing

filterThread strvar chvar ctx action = do
  state <- newIORef ("", [])
  forever $ handle (\StopComputing -> return ()) $ do
    str <- takeMVar strvar
    chars <- readMVar chvar
    putStrLn "Filter start"
    (str', res) <- filterChars state chars str
    putStrLn $ "Filter end: " ++ show (length res)
    runIn ctx $ do
      writeIORef state (str', res)
      action res

parSearch thread var str = do
  throwTo thread StopComputing
  putMVar var str


filterChars var initChars str' = do
  st@(str, chars) <- readIORef var
  let res = filter (strMatcher str') chars' `using` evalList rseq
      chars' = if str `isPrefixOf` str' && not (null str)
               then chars else initChars
  return (str', res)
