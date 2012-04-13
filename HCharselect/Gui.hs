{-# LANGUAGE PackageImports #-}
module HCharselect.Gui (gui) where
import Graphics.UI.Gtk
import Data.List
import Control.Monad
import "mtl" Control.Monad.Trans
import System.Process
import System.IO
import System.Environment

import Control.Concurrent

import HCharselect.Concurrency
import HCharselect.Parser

windowHeight = 500
windowWidth  = 500

gui chars = do
  initGUI
  window <- windowNew
  vbox <- vBoxNew False 0
  entry <- entryNew
  scroll <- scrolledWindowNew Nothing Nothing
  charModel <- listStoreNew []
  charList <- treeViewNewWithModel charModel

  ctx <- newCompCtx
  timeoutAddFull (execCtx ctx >> return True) priorityDefaultIdle 50

  set window [ widgetWidthRequest := windowWidth
             , widgetHeightRequest := windowHeight
             , windowWindowPosition := WinPosCenter
             , windowTypeHint := WindowTypeHintDialog
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

  -- Incremental Search Thread
  incVar <- newEmptyMVar
  incThread <- forkIO $ filterThread incVar chars ctx $ \res -> do
    listStoreClear charModel
    mapM_ (listStoreAppend charModel) res
  let incSearch = parSearch incThread incVar

      copyCurrent path = do
        (Just iter) <- treeModelGetIter charModel path
        let idx = listStoreIterToIndex iter
        Character _ char _ <- listStoreGetValue charModel idx
        runXClip [char]

  onDestroy window mainQuit

  onEditableChanged entry $ do
    text <- entryGetText entry
    when (length text >= 3) $
      incSearch text

  onEntryActivate entry $ do
    text <- entryGetText entry
    incSearch text

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

  charList `on` keyPressEvent $ tryEvent $ do
    "Return" <- eventKeyName
    [Shift] <- eventModifier
    (path,_) <- liftIO $ treeViewGetCursor charList
    liftIO $ copyCurrent path

  onRowActivated charList $ \path col -> do
    copyCurrent path
    mainQuit
    
  widgetShowAll window
  mainGUI

addCol title model fun = do
  col <- treeViewColumnNew
  treeViewColumnSetTitle col title
  renderer <- cellRendererTextNew
  cellLayoutPackStart col renderer False
  cellLayoutSetAttributes col renderer model $ \row ->
    [cellText := fun row]
  return col

characterName (Character n _ _) = n
characterChar (Character _ c _) = [c]
characterAliases (Character _ _ as) = intercalate ", " as


runXClip :: String -> IO ()
runXClip string = do
  (input,_,_,pid) <- runInteractiveProcess "xclip" ["-i"] Nothing Nothing
  hPutStr input string
  hClose input
  waitForProcess pid >> return ()
