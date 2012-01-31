module HCharselect.CommonOptions
       ( CommonConfig(CommonConf)
       , CommonOptDesc 
       , defaultConfig
       , setOther
       , usage
       , version
       , commonOptions
       , helpRequested
       , versionRequested
       , dataFile
       , other
       , parseOpts
       , maybeShowHelp 
       ) where

import System.Console.GetOpt
import System.Environment
import System.Exit

usage name rest = "Usage: " ++ name ++ " " ++ rest ++ "\n"
version         = "This is hcharselect version 0.1"
defaultDataFile = "/usr/share/apps/kcharselect/kcharselect-data"

data CommonConfig a = CommonConf {
  helpRequested    :: Bool,
  versionRequested :: Bool,
  dataFile         :: FilePath,
  other            :: a
} deriving (Show)

defaultConfig a = CommonConf {
  helpRequested    = False,
  versionRequested = False,
  dataFile         = defaultDataFile,
  other            = a
}

type CommonOptDesc a = OptDescr (CommonConfig a -> CommonConfig a)

commonOptions :: [CommonOptDesc a]
commonOptions =
  [ Option ['v'] ["version"]   (NoArg setVersionReq)          "version information"
  , Option ['h'] ["help"]      (NoArg setHelpReq)             "this help"
  , Option ['d'] ["data-file"] (ReqArg setDataFile "FILE")    "kcharselect-data file"
  --, Option []    ["resizable"] (NoArg Resizable) "let the window be resizable"
  ]


setVersionReq conf    = conf { versionRequested = True }
setHelpReq conf       = conf { helpRequested = True }
setDataFile path conf = conf { dataFile = path }
setOther f conf       = conf { other = f $ other conf }


parseOpts :: [String] -> CommonConfig a -> [CommonOptDesc a]
             -> (CommonConfig a, [String], [String])
parseOpts argv def optdefs = (conf, nonopts, errs)
  where (opts, nonopts, errs) = getOpt Permute optdefs argv
        conf = foldl (flip id) def opts

maybeShowHelp :: CommonConfig a -> [CommonOptDesc a] -> String -> IO (CommonConfig a)
maybeShowHelp conf@(CommonConf help vers _ _) opts usg
  | help      = do name <- getProgName
                   putStr $ usageInfo (usage name usg) opts
                   exitWith ExitSuccess
  | vers      = putStrLn version >> exitWith ExitSuccess
  | otherwise = return conf

