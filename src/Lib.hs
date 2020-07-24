{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Lib
  ( runUI
  , WIEvent (..)
  , ConnectionStatus (..)
  ) where

import           Brick
import           Brick.AttrMap              (AttrMap, attrMap)
import           Brick.BChan
import           Brick.Types                (handleEventLensed)
import           Brick.Util                 (fg, on)
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
import           Data.Time.Format
import           Graphics.Vty               (Attr, black, blue, cyan, green, red, white, withURL,
                                             yellow)
import           Graphics.Vty.Attributes    (defAttr)
import           Lens.Micro
import           Lens.Micro.TH

import qualified Brick.Widgets.List         as BrickL
import qualified Data.Text                  as T
import qualified Data.Vector                as Vec
import qualified Graphics.Vty               as Vty

data ResourceName
  = RNMessageEditor
  | RNServerBox
  | RNClientBox
  deriving (Show, Eq, Ord)

data ConnectionStatus
  = CSConnecting
  | CSError !Text
  | CSConnected
  | CSClosed
  deriving (Show)

-- | The state we want to maintain
data AppState
  = AppState
  { _asClientMessages   :: !(BrickL.List ResourceName (UTCTime, Text))
  , _asServerMessages   :: !(BrickL.List ResourceName (UTCTime, Text))
  , _asMessageEditor    :: !(Editor Text ResourceName)
  , _asConnectionStatus :: !ConnectionStatus
  } deriving (Show)

makeLenses 'AppState

-- | our custom event
data WIEvent
  = WIENewServerMsg !Text !UTCTime
  | WIENewClientMsg !Text !UTCTime
  | WIEConnectionUpdate !ConnectionStatus
  deriving (Show)

drawMessageBox :: Text -> AttrName -> BrickL.List ResourceName (UTCTime, Text) -> Widget ResourceName
drawMessageBox label attrName msgs =
  borderWithLabel (txt label) $ BrickL.renderList renderMsg False msgs
  where
    -- TODO: strip message of escape sequence, tab and carriage return. see
    -- https://hackage.haskell.org/package/brick-0.55/docs/Brick-Widgets-Core.html#v:txt
    renderMsg _selected (time, msg) =
      withAttr attrName (txt "> ") <+> txtWrap (timestamp time <> msg)
    timestamp t =
      let fmtTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" t
      in "[" <> T.pack fmtTime <> "] "

drawReadingBox :: AppState -> Widget ResourceName
drawReadingBox (AppState clientMsgs serverMsgs _ _) =
  center (drawMessageBox "Messages from Server" tsServer serverMsgs)
  <+> center (drawMessageBox "Messages sent by you (client)" tsClient clientMsgs)

drawInputBox :: AppState -> Widget ResourceName
drawInputBox state =
  withBorderStyle unicode $ borderWithLabel (txt "Message Editor") $
  txt "> " <+> renderEditor (txt . T.unlines) True (state ^. asMessageEditor)

drawStats :: AppState -> Widget ResourceName
drawStats AppState{..} =
  let status = case _asConnectionStatus of
        CSConnecting -> "Connecting"
        CSError err  -> "Error: " <> err
        CSConnected  -> "Connected"
        CSClosed     -> "Connection closed by server"
  in withAttr statusHud $ withBorderStyle unicode $
     borderWithLabel (txt "Status") $ txt status

drawBottomControl :: AppState -> Widget ResourceName
drawBottomControl appState = drawInputBox appState <+> drawStats appState

footerHelp :: Widget ResourceName
footerHelp = txt "HELP: **ESC**: quit/exit | **ENTER**: Send message | Timestamps are in UTC."

drawUI :: AppState -> [Widget ResourceName]
drawUI appState =
  [drawReadingBox appState <=> drawBottomControl appState <=> footerHelp]

handleEvent :: BChan Text -> AppState -> BrickEvent ResourceName WIEvent -> EventM ResourceName (Next AppState)
handleEvent clientChan state@AppState{..} ev = case ev of
  -- handle custom events
  AppEvent aev -> case aev of
    WIENewClientMsg m t -> let newList = insertNewMessage (t,m) _asClientMessages
                           in continue $ state { _asClientMessages = newList }
    WIENewServerMsg m t -> let newList = insertNewMessage (t,m) _asServerMessages
                           in continue $ state { _asServerMessages = newList }
    WIEConnectionUpdate status -> continue $ state { _asConnectionStatus = status }
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
      , appAttrMap = styleMap
      , appChooseCursor = showFirstCursor
      }

-- globalDefault :: Attr
-- globalDefault = white `on` black

tsServer = "ts-server"
tsClient = "ts-client"
statusHud = "connection-status"

styleMap :: AppState -> AttrMap
styleMap state = attrMap defAttr
  [ (tsServer, fg blue)
  , (tsClient, fg green)
  , (statusHud, mkStatusHudStyle)
  ]
  where
    mkStatusHudStyle = case state ^. asConnectionStatus of
      CSConnecting -> fg yellow
      CSError _    -> fg red
      CSClosed     -> fg red
      CSConnected  -> fg green

runUI :: BChan WIEvent -> BChan Text -> IO ()
runUI brickChan clientChan = do
  let initialState = AppState clientList serverList (editor RNMessageEditor (Just 1) "") CSConnecting
      clientList = BrickL.list RNClientBox Vec.empty 1
      serverList = BrickL.list RNServerBox Vec.empty 1
      buildVty = Vty.mkVty Vty.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just brickChan) (mkApp clientChan) initialState
