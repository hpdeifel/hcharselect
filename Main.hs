{-# LANGUAGE BangPatterns #-}

module Main (main) where
import Graphics.UI.Gtk
import Parser
import Data.List
import Control.Monad
import Data.Char
import Control.Concurrent
import Control.Monad.Trans
import System.Process
import System.IO
import Control.DeepSeq
import System.Environment
import Control.Exception

windowHeight = 500
windowWidth  = 500
defaultFile  = "/usr/share/apps/kcharselect/kcharselect-data"

maybeParse file =
  do chars <- try (parseFile file) :: IO (Either SomeException [Character])
     case chars of
       Right a -> return a
       Left  a -> hPutStrLn stderr (show a) >> return []

parseThread var file = do
  putStrLn "Parsing started"
  chars <- maybeParse file
  chars `deepseq` putStrLn "Parsing done"
  putMVar var chars

usage = do
  self <- getProgName
  hPutStrLn stderr ("Usage: " ++ self ++ " [kcharselect-file]")

getFilename = getArgs >>= \args -> do
  case args of
    [file]    -> return file
    []        -> return defaultFile
    otherwise -> usage >> fail "Could not parse args"

main = do
  filename <- getFilename
  chars <- newEmptyMVar
  forkIO $ parseThread chars filename
  initGUI
  window <- windowNew
  vbox <- vBoxNew False 0
  entry <- entryNew
  scroll <- scrolledWindowNew Nothing Nothing
  charModel <- listStoreNew []
  charList <- treeViewNewWithModel charModel

  set window [ widgetWidthRequest := windowWidth
             , widgetHeightRequest := windowHeight
             , windowResizable := False
             , windowWindowPosition := WinPosCenter
             , windowTitle := "Charselect"
             ]

  containerAdd window vbox
  boxPackStart vbox entry PackNatural 0
  boxPackStart vbox scroll PackGrow 0
  containerAdd scroll charList

  col2 <- addCol "Char" charModel characterChar
  col1 <- addCol "Name" charModel characterName
  treeViewAppendColumn charList col2
  treeViewAppendColumn charList col1

  onDestroy window mainQuit

  onEntryActivate entry $ do
    listStoreClear charModel
    text <- entryGetText entry
    chrs <- readMVar chars
    mapM_ (listStoreAppend charModel) (filterChars text chrs)

  charList `on` keyPressEvent $ tryEvent $ do
    "j" <- eventKeyName
    (path,_) <- liftIO $ treeViewGetCursor charList
    Just iter <- liftIO $ treeModelGetIter charModel path
    (Just next) <- liftIO $ treeModelIterNext charModel iter
    path2 <- liftIO $ treeModelGetPath charModel next
    liftIO $ treeViewSetCursor charList path2 Nothing

  charList `on` keyPressEvent $ tryEvent $ do
    "k" <- eventKeyName
    (path,_) <- liftIO $ treeViewGetCursor charList
    Just (TreeIter a b c d) <- liftIO $ treeModelGetIter charModel path
    let prev = TreeIter a (if b == 0 then 0 else b-1) c d
    path2 <- liftIO $ treeModelGetPath charModel prev
    liftIO $ treeViewSetCursor charList path2 Nothing

  onRowActivated charList $ \path col -> do
    (Just iter) <- treeModelGetIter charModel path
    let idx = listStoreIterToIndex iter
    Character _ char <- listStoreGetValue charModel idx
    runXClip [char]
    mainQuit
    
  widgetShowAll window
  mainGUI

filterChars :: String -> [Character] -> [Character]
filterChars text chars = filter substring chars
  where substring (Character name _) = substrIngore " ,.\t'-_" (map toUpper text) name

substrIngore ignbag term text = isInfixOf (remBag term) (remBag text)
  where remBag = filter (not . flip elem ignbag)

addCol title model fun = do
  col <- treeViewColumnNew
  treeViewColumnSetTitle col title
  renderer <- cellRendererTextNew
  cellLayoutPackStart col renderer False
  cellLayoutSetAttributes col renderer model $ \row ->
    [cellText := (fun row)]
  return col

characterName (Character n _) = n
characterChar (Character _ c) = [c]


runXClip :: String -> IO ()
runXClip string = do
  (input,_,_,pid) <- runInteractiveProcess "xclip" ["-i"] Nothing Nothing
  hPutStr input string
  hClose input
  waitForProcess pid >> return ()
