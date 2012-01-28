{-# LANGUAGE BangPatterns #-}

module HCharselect.Gui (gui) where
import Graphics.UI.Gtk
import HCharselect.Parser
import Data.List
import Control.Monad
import Data.Char
import Control.Concurrent
import Control.Monad.Trans
import System.Process
import System.IO
import System.Environment

windowHeight = 500
windowWidth  = 500

gui chars resizable = do
  initGUI
  window <- windowNew
  vbox <- vBoxNew False 0
  entry <- entryNew
  scroll <- scrolledWindowNew Nothing Nothing
  charModel <- listStoreNew []
  charList <- treeViewNewWithModel charModel

  set window [ widgetWidthRequest := windowWidth
             , widgetHeightRequest := windowHeight
             , windowResizable := resizable
             , windowWindowPosition := WinPosCenter
             , windowTitle := "Charselect"
             ]

  containerAdd window vbox
  boxPackStart vbox entry PackNatural 0
  boxPackStart vbox scroll PackGrow 0
  containerAdd scroll charList

  col2 <- addCol "Char" charModel characterChar
  col1 <- addCol "Name" charModel characterName
  col3 <- addCol "Aliases" charModel characterAliases
  treeViewAppendColumn charList col2
  treeViewAppendColumn charList col1
  treeViewAppendColumn charList col3

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
    Character _ char _ <- listStoreGetValue charModel idx
    runXClip [char]
    mainQuit
    
  widgetShowAll window
  mainGUI

filterChars :: String -> [Character] -> [Character]
filterChars text chars = filter substring chars
  where substring (Character name _ aliases) = any match (name:aliases)
        match = substrIngore " ,.\t'-_" (map toUpper text) . (map toUpper)

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

characterName (Character n _ _) = n
characterChar (Character _ c _) = [c]
characterAliases (Character _ _ as) = concat $ intersperse ", " as


runXClip :: String -> IO ()
runXClip string = do
  (input,_,_,pid) <- runInteractiveProcess "xclip" ["-i"] Nothing Nothing
  hPutStr input string
  hClose input
  waitForProcess pid >> return ()
