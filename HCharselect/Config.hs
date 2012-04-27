{-# LANGUAGE ScopedTypeVariables #-}
module HCharselect.Config 
       ( Config(..)
       , emptyConfig
       , parseConfig
       , parseConfig'
       , parseConfigFile
       , configFileLocation
       ) where

import Prelude hiding (catch)

import qualified Data.Map as M
import Data.List

import Control.Exception
import System.Environment (getEnv)
import System.IO

import HCharselect.Utils

-------------------------
-- Datatype definition --
-------------------------

data Config = Config {
  confVars    :: M.Map String String,
  confAliases :: M.Map String (Either String Int)
} deriving (Show)

emptyConfig = Config M.empty M.empty

--------------------------
-- Config file location --
--------------------------

configFileLocation :: IO FilePath
configFileLocation = (++"/hcharselect/config") `fmap` xdgConfigHome

xdgConfigHome :: IO FilePath
xdgConfigHome = do
  xdgHome <- getEnv' "XDG_CONFIG_HOME"
  home    <- getEnv "HOME"
  case xdgHome of
    Just path -> return path
    Nothing   -> return (home ++ "/.config")

--------------------
-- Actual parsing --
--------------------

-- like parseConfig, but used the default file location
parseConfig' :: IO Config
parseConfig' = configFileLocation >>= parseConfig

-- Parses the config file and catches errors
parseConfig :: FilePath -> IO Config
parseConfig path = parseConfigFile path
                   `catch` \(_ :: SomeException) -> return emptyConfig

-- May throw exceptions
parseConfigFile :: FilePath -> IO Config
parseConfigFile path = withFile path ReadMode $ \handle -> do
  contents <- hGetContents handle
  parseLines emptyConfig 0 $ lines contents

parseLines :: Config -> Int -> [String] -> IO Config
parseLines config _ [] = return config
parseLines config i (l:ls) = do
  config' <- parseLine config i (strip l)
  parseLines config' (i+1) ls

parseLine :: Config -> Int -> String -> IO Config
parseLine conf _ ('#':_)  = return conf -- comment, ignore whole line
parseLine conf _ ""       = return conf -- empty line: ignore, too
parseLine conf linum line = case M.lookup command lineHandlers of
  Just handler -> handler conf rest
  Nothing      -> logMsg ("Unknown command " ++ command) >> return conf

  where (command, rest) = (splitFirstWord line)


lineHandlers :: M.Map String (Config -> String -> IO Config)
lineHandlers = M.fromList [
  ("alias", parseAlias),
  ("set"  , parseVariable) ]
  
parseAlias :: Config -> String -> IO Config
parseAlias conf str = case splitFirstWord str of
  ("", _) -> do
    logMsg ("Couldn't parse alias line: alias " ++ str)
    return conf
  (name, alias) ->
    let val = maybe (Left alias) Right (parseInt alias)
    in return $ conf { confAliases = M.insert name val (confAliases conf)}


parseVariable :: Config -> String -> IO Config
parseVariable conf str = case splitFirstWord str of
  ("", _) -> do
    logMsg ("Couldn't parse set line: set " ++ str)
    return conf
  (name, val) ->
    return $ conf { confVars = M.insert name val (confVars conf) }
    

---------------
-- Utilities --
---------------

isWhitespace :: Char -> Bool
isWhitespace c = c `elem` [' ', '\t']

getEnv' :: String -> IO (Maybe String)
getEnv' var = (Just `fmap` getEnv var) `catch` \(_ :: SomeException) -> return Nothing

logMsg :: String -> IO ()
logMsg = putStrLn

strip = dropWhile isWhitespace

splitFirstWord str = (first, rest')
  where (first, rest) = break isWhitespace str
        rest'         = strip rest
