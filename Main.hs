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

windowHeight = 500
windowWidth  = 500

parseThread var = do
  putStrLn "Parsing started"
  chars <- parseFile "/usr/share/apps/kcharselect/kcharselect-data"
  chars `deepseq` putStrLn "Parsing done"
  putMVar var chars

main = do
  chars <- newEmptyMVar
  forkIO $ parseThread chars
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

  onRowActivated charList $ \path col -> do
    (Just iter) <- treeModelGetIter charModel path
    let idx = listStoreIterToIndex iter
    Character _ char <- listStoreGetValue charModel idx
    runXClip [char]
    mainQuit
    
  widgetShowAll window
  mainGUI

filterChars text chars = filter substring chars
  where substring (Character name _) = isInfixOf (map toUpper text) name

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
