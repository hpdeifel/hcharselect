{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where
import System.Console.GetOpt
import System.Environment
import System.Exit
import Control.DeepSeq
import Control.Exception
import Control.Concurrent
import System.IO

import HCharselect.Gui
import HCharselect.Parser
import HCharselect.Concurrency

usage name      = "Usage: " ++ name ++ " [OPTIONS] [-d data-file]\n"
version         = "This is hcharselect version 0.1"
defaultDataFile = "/usr/share/apps/kcharselect/kcharselect-data"


data Flag = Version | Help | DataFile FilePath | Resizable
          deriving (Show)

options =
  [ Option ['v'] ["version"]   (NoArg Version)          "version information"
  , Option ['h'] ["help"]      (NoArg Help)             "this help"
  , Option ['d'] ["data-file"] (ReqArg DataFile "FILE") "kcharselect-data file"
  , Option []    ["resizable"] (NoArg Resizable) "let the window be resizable"
  ]



data Config = Config {
  dataFile :: FilePath,
  resizable :: Bool
} deriving (Show)

mkConf :: [Flag] -> IO Config
mkConf [] = return $ Config { dataFile = defaultDataFile, resizable = False}
mkConf (Version:_) = do
  putStrLn version
  exitWith ExitSuccess
mkConf (Help:_) = do
  name <- getProgName
  putStr $ usageInfo (usage name) options
  exitWith ExitSuccess
mkConf (DataFile f:r) = do 
  conf <- mkConf r
  return $ conf { dataFile = f }
mkConf (Resizable:r) = do
  conf <- mkConf r
  return $ conf { resizable = True }



parseOpts :: [String] -> IO Config
parseOpts argv =
  case getOpt Permute options argv of
    (o, [], [])  -> mkConf o
    (_, n, [])   -> ioError (userError $ "superfluous args: " ++ concat n)
    (_, _, errs) -> ioError (userError $ concat errs)



main = do
  name <- getProgName
  args <- getArgs
  
  conf <- parseOpts args

  chars <- newEmptyMVar
  forkIO $ parseThread chars (dataFile conf)
  gui chars (resizable conf)
