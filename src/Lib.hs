{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Lib
  ( runUI
  , WIEvent (..)
  ) where

import           Brick
import           Brick.BChan
import           Brick.Types                (handleEventLensed)
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import           Brick.Widgets.Edit
import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Monad              (forever, void)
import           Control.Monad.IO.Class
import           Data.Text                  (Text)
import           Data.Text.Zipper
import           Data.Time.Clock
import           Graphics.Vty.Attributes    (defAttr)
import           Lens.Micro
import           Lens.Micro.TH

import qualified Brick.Widgets.List         as BrickL
import qualified Data.Text                  as T
import qualified Data.Vector                as Vec
import qualified Graphics.Vty               as Vty

type ResourceName = Text

-- | The state we want to maintain
data AppState
  = AppState
  { _asClientMessages :: !(BrickL.List ResourceName (UTCTime, Text))
  , _asServerMessages :: !(BrickL.List ResourceName (UTCTime, Text))
  , _asMessageEditor  :: !(Editor Text ResourceName)
  } deriving (Show)

makeLenses 'AppState

-- | our custom event
data WIEvent
  = WIENewServerMsg !Text !UTCTime
  | WIENewClientMsg !Text !UTCTime
  deriving (Show)

drawMessageBox :: Text -> BrickL.List ResourceName (UTCTime, Text) -> Widget ResourceName
drawMessageBox label msgs =
  borderWithLabel (txt label) $ BrickL.renderList renderMsg False msgs
  where
    renderMsg _selected (time, msg) = txtWrap $ "[" <> (T.pack $ show time) <> "] " <> msg
    vBoxFill ws = vBox $ ws <> [fill ' ']

drawReadingBox :: AppState -> Widget ResourceName
drawReadingBox (AppState clientMsgs serverMsgs _) =
  withBorderStyle unicode $
  (    center (drawMessageBox "Messages from Server" serverMsgs)
   <+> center (drawMessageBox "Messages sent by you (client)" clientMsgs)
  )

drawInputBox :: AppState -> Widget ResourceName
drawInputBox state =
  withBorderStyle unicode $ borderWithLabel (txt "Message Editor") $
  txt "> " <+> renderEditor (txt . T.unlines) True (state ^. asMessageEditor)

footerHelp :: Widget ResourceName
footerHelp = txt "HELP: **ESC**: quit/exit | **ENTER**: Send message"

drawUI :: AppState -> [Widget ResourceName]
drawUI appState = [drawReadingBox appState <=> drawInputBox appState <=> footerHelp]

handleEvent :: BChan Text -> AppState -> BrickEvent ResourceName WIEvent -> EventM ResourceName (Next AppState)
handleEvent clientChan state@AppState{..} ev = case ev of
  -- handle custom events
  AppEvent aev -> case aev of
    WIENewClientMsg m t -> let newList = insertNewMessage (t,m) _asClientMessages
                           in continue $ state { _asClientMessages = newList }
    WIENewServerMsg m t -> let newList = insertNewMessage (t,m) _asServerMessages
                           in continue $ state { _asServerMessages = newList }
  -- handle Vty key events
  VtyEvent vev -> case vev of
    Vty.EvKey Vty.KEsc []   -> halt state
    Vty.EvKey Vty.KEnter [] -> handleEnter state clientChan
    _                       -> handleEventLensed state asMessageEditor handleEditorEvent vev
                               >>= \st -> handleEventLensed st asClientMessages BrickL.handleListEvent vev
                               >>= \st -> handleEventLensed st asServerMessages BrickL.handleListEvent vev
                               >>= continue
  _            -> continue state

insertNewMessage
  :: a
  -> BrickL.GenericList n Vec.Vector a
  -> BrickL.GenericList n Vec.Vector a
insertNewMessage elem msgList =
 let pos = Vec.length $ msgList ^. BrickL.listElementsL
 in BrickL.listMoveDown $ BrickL.listInsert pos elem msgList

handleEnter :: AppState -> BChan Text -> EventM n (Next AppState)
handleEnter state clientChan = do
  let contents = T.unlines $ getEditContents $ state ^. asMessageEditor
      newState = state & asMessageEditor .~ (resetEdit state)
  if T.null $ T.strip contents
    then continue state
    else do
    liftIO $ writeBChan clientChan contents
    continue newState
  where
    -- resetEdit :: AppState -> Editor Text ResourceName
    resetEdit st = applyEdit transformer $ state ^. asMessageEditor
    -- transformer :: TextZipper Text -> TextZipper Text
    transformer _ = textZipper [] (Just 1)

mkApp :: BChan Text -> App AppState WIEvent ResourceName
mkApp clientChan =
  App { appDraw = drawUI
      , appHandleEvent = handleEvent clientChan
      , appStartEvent = return
      , appAttrMap = const $ attrMap defAttr []
      , appChooseCursor = showFirstCursor
      }

runUI :: BChan WIEvent -> BChan Text -> IO ()
runUI brickChan clientChan = do
  let initialState = AppState clientList serverList (editor "edit-box" (Just 1) "")
      clientList = BrickL.list "client-list" Vec.empty 1
      serverList = BrickL.list "server-list" Vec.empty 1
      buildVty = Vty.mkVty Vty.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just brickChan) (mkApp clientChan) initialState
