{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module TUI
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
import           Control.Monad              (forever, void, when)
import           Control.Monad.IO.Class
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text)
import           Data.Text.Zipper
import           Data.Time.Clock
import           Data.Time.Format
import           Graphics.Vty               (Attr, black, blue, cyan, green, red, rgbColor, white,
                                             withURL, yellow)
import           Graphics.Vty.Attributes    (defAttr)
import           Lens.Micro
import           Lens.Micro.TH

import qualified Brick.Widgets.List         as BrickL
import qualified Data.Text                  as T
import qualified Data.Vector                as Vec
import qualified Graphics.Vty               as Vty

grey = rgbColor 102 102 102

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
  , _asHistoryIndex     :: !(Maybe Int)
  } deriving (Show)

makeLenses 'AppState

-- | our custom event
data WIEvent
  = WIENewServerMsg !Text !UTCTime
  | WIENewClientMsg !Text !UTCTime
  | WIEConnectionUpdate !ConnectionStatus
  deriving (Show)

data HistoryDir = HDUp | HDDown

drawMessageBox :: Widget ResourceName -> AttrName -> BrickL.List ResourceName (UTCTime, Text) -> Widget ResourceName
drawMessageBox labelWidget attrName msgs =
  borderWithLabel labelWidget $ BrickL.renderList renderMsg False msgs
  where
    -- TODO: strip message of escape sequence, tab and carriage return. see
    -- https://hackage.haskell.org/package/brick-0.55/docs/Brick-Widgets-Core.html#v:txt
    renderMsg _selected (time, msg) =
      withAttr attrName (txt "> ") <+> txtWrap (timestamp time <> msg)
    timestamp t =
      let fmtTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S.%3q" t
      in "[" <> T.pack fmtTime <> "] "

drawReadingBox :: AppState -> Widget ResourceName
drawReadingBox AppState{..} =
  center (drawMessageBox serverLabel tsServer _asServerMessages)
  <+> center (drawMessageBox clientLabel tsClient _asClientMessages)
  where
    serverLabel = withAttr serverBoxLabel $ txt "Messages from Server"
    clientLabel = withAttr clientBoxLabel $ txt "Messages sent"

drawInputBox :: AppState -> Widget ResourceName
drawInputBox state =
  withBorderStyle unicodeBold $ borderWithLabel (txt "Message Editor") $
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
footerHelp = withAttr footerAttr $ txt "**ESC**: quit/exit | **ENTER**: Send message | **Up/Down**: Message History | Timestamps are in UTC."

drawUI :: AppState -> [Widget ResourceName]
drawUI appState =
  [drawReadingBox appState <=> drawBottomControl appState <=> footerHelp]

handleEvent
  :: BChan Text
  -> AppState
  -> BrickEvent ResourceName WIEvent
  -> EventM ResourceName (Next AppState)
handleEvent clientChan state@AppState{..} ev = do
  vty <- getVtyHandle
  let output = Vty.outputIface vty
  when (Vty.supportsMode output Vty.BracketedPaste) $
    liftIO $ Vty.setMode output Vty.BracketedPaste True
  case ev of
    -- handle custom events
    AppEvent aev -> case aev of
      WIENewClientMsg m t -> let newList = insertNewItem (t,m) _asClientMessages
                            in continue $ state { _asClientMessages = newList }
      WIENewServerMsg m t -> let newList = insertNewItem (t,m) _asServerMessages
                            in continue $ state { _asServerMessages = newList }
      WIEConnectionUpdate status -> continue $ state { _asConnectionStatus = status }
    -- handle Vty key events
    VtyEvent vev -> case vev of
      Vty.EvKey Vty.KEsc []   -> halt state
      Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl]  -> halt state
      Vty.EvKey Vty.KEnter [] -> handleEnter state clientChan
      Vty.EvKey Vty.KUp []    -> handleHistorySearch state HDUp
      Vty.EvKey Vty.KDown []  -> handleHistorySearch state HDDown
      _                       -> handleEventLensed state asMessageEditor handleEditorEvent vev
                                 >>= continue
    _            -> continue state

handleHistorySearch :: AppState -> HistoryDir -> EventM n (Next AppState)
handleHistorySearch st@AppState{..} dir
  | Vec.null (_asClientMessages ^. BrickL.listElementsL) = continue st
  | otherwise = do
  let list = _asClientMessages ^. BrickL.listElementsL
      lastIdx = max 0 (Vec.length list - 1)
      newIdx = case (_asHistoryIndex, dir) of
        (Nothing, _)       -> lastIdx
        (Just idx, HDUp)   -> max 0 (idx - 1)
        (Just idx, HDDown) -> min lastIdx (idx + 1)
      message = fromMaybe "" $ fmap snd $ list Vec.!? newIdx
      newEdit = updateEditor _asMessageEditor [message]

  continue $ st { _asHistoryIndex = Just newIdx, _asMessageEditor = newEdit }

updateEditor :: Editor Text n -> [Text] -> Editor Text n
updateEditor editor content =
  applyEdit (const $ textZipper content (Just 1)) $ editor

handleEnter :: AppState -> BChan Text -> EventM n (Next AppState)
handleEnter state clientChan = do
  let contents = T.unlines $ getEditContents $ state ^. asMessageEditor
      newState = state & asMessageEditor .~ (resetEdit state)
                       & asHistoryIndex .~ Nothing -- reset the history index
  if T.null $ T.strip contents
    then continue state
    else do
    liftIO $ writeBChan clientChan contents
    continue newState
  where
    resetEdit st = applyEdit clearZipper $ state ^. asMessageEditor

mkApp :: BChan Text -> App AppState WIEvent ResourceName
mkApp clientChan =
  App { appDraw = drawUI
      , appHandleEvent = handleEvent clientChan
      , appStartEvent = return
      , appAttrMap = styleMap
      , appChooseCursor = const $ showCursorNamed RNMessageEditor
      }

tsServer = "ts-server"
tsClient = "ts-client"
statusHud = "connection-status"
serverBoxLabel = "server-box-label"
clientBoxLabel = "client-box-label"
footerAttr = "footer"

styleMap :: AppState -> AttrMap
styleMap state = attrMap defAttr
  [ (tsServer, fg blue)
  , (tsClient, fg green)
  , (statusHud, mkStatusHudStyle)
  , (serverBoxLabel, fg blue)
  , (clientBoxLabel, fg green)
  , (footerAttr, fg grey)
  ]
  where
    -- globalDefault = white `on` black
    mkStatusHudStyle = case state ^. asConnectionStatus of
      CSConnecting -> fg yellow
      CSError _    -> fg red
      CSClosed     -> fg red
      CSConnected  -> fg green

initialState :: AppState
initialState =
  let clientList = BrickL.list RNClientBox Vec.empty 1
      serverList = BrickL.list RNServerBox Vec.empty 1
  in AppState clientList serverList (editor RNMessageEditor (Just 1) "") CSConnecting Nothing

runUI :: BChan WIEvent -> BChan Text -> IO ()
runUI brickChan clientChan = do
  let buildVty = Vty.mkVty Vty.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just brickChan) (mkApp clientChan) initialState

-- | inserts a new item to a 'BrickL.List', and moves the list down to the latest element
insertNewItem :: a -> BrickL.List n a -> BrickL.List n a
insertNewItem elem list =
 let pos = Vec.length $ list ^. BrickL.listElementsL
 in BrickL.listMoveDown $ BrickL.listInsert pos elem list
