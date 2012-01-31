module Main (main) where

import System.Environment
import Control.Monad
import Data.List

import HCharselect.Parser
import HCharselect.Cli
import HCharselect.CommonOptions


data CliConfig = CliConf { searchTerm :: String }
defaultCliConfig = CliConf ""

parseOptsIO :: [String] -> IO (CommonConfig CliConfig)
parseOptsIO argv = case parseOpts argv (defaultConfig defaultCliConfig) commonOptions of
    (c, search, [])  -> do c' <- maybeShowHelp c commonOptions "[OPTIONS] PATTERN..."
                           return $ setOther (const $ CliConf $ intercalate " " search) c'
    (_, _, errs) -> ioError (userError $ concat errs)

main = do
  args <- getArgs
  conf <- parseOptsIO args
  when (not . null . searchTerm $ other conf) $ do
    chars <- parseFile (dataFile conf)
    cli chars (searchTerm $ other conf)
