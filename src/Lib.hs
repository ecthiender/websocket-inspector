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

import qualified Data.Text                  as T
import qualified Graphics.Vty               as Vty

type ResourceName = Text

-- | The state we want to maintain
data AppState
  = AppState
  { _asClientMessages :: ![(UTCTime, Text)]
  , _asServerMessages :: ![(UTCTime, Text)]
  , _asMessageEditor  :: !(Editor Text ResourceName)
  } deriving (Show)

makeLenses 'AppState

-- | our custom event
data WIEvent
  = WIENewServerMsg !Text !UTCTime
  | WIENewClientMsg !Text !UTCTime
  deriving (Show)

drawMessageBox :: Text -> [(UTCTime, Text)] -> Widget ResourceName
drawMessageBox label msgs =
  borderWithLabel (txt label) $ padAll 1 $ vBoxFill $ (map renderMsg $ reverse msgs)
  where
    renderMsg (time, msg) = txtWrap $ "[" <> (T.pack $ show time) <> "] " <> msg
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
    WIENewClientMsg m t -> continue $ state { _asClientMessages = _asClientMessages ++ [(t, m)] }
    -- cap the no of server messages kept in memory to be 50, because anyway so many won't be visible
    WIENewServerMsg m t -> let newList = if length _asServerMessages > 50
                                         then (tail _asServerMessages) ++ [(t, m)]
                                         else _asServerMessages ++ [(t, m)]
                               newState = state { _asServerMessages = newList }
                           in continue newState
  -- handle Vty key events
  VtyEvent vev -> case vev of
    Vty.EvKey Vty.KEsc []   -> halt state
    Vty.EvKey Vty.KEnter [] -> handleEnter
    _                       -> handleEventLensed state asMessageEditor handleEditorEvent vev >>= continue
  _            -> continue state

  where
    handleEnter = do
      let contents = T.unlines $ getEditContents $ state ^. asMessageEditor
          newState = state & asMessageEditor .~ (resetEdit state)
      if T.null $ T.strip contents
        then continue state
        else do
        liftIO $ writeBChan clientChan contents
        continue newState

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
  let initialState = AppState [] [] (editor "edit-box" (Just 1) "")
      buildVty = Vty.mkVty Vty.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just brickChan) (mkApp clientChan) initialState
