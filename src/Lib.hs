{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Lib
  ( runUI
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
  { _asClientMessages    :: ![Text]
  , _asServerMessages    :: ![Text]
  , _asCurrentEditorText :: !(Editor Text ResourceName)
  } deriving (Show)

makeLenses 'AppState

-- | our custom event
data WIEvent
  = WIENewServerMsg !Text
  | WIENewClientMsg !Text
  deriving (Show)

drawMessageBox :: [Text] -> Widget ResourceName
drawMessageBox msgs = padAll 2 $ vBox $ map txt msgs

drawReadingBox :: AppState -> Widget ResourceName
drawReadingBox (AppState clientMsgs serverMsgs _) =
  withBorderStyle unicode $ border $
  (center (drawMessageBox serverMsgs) <+> vBorder <+> center (drawMessageBox clientMsgs))

drawInputBox :: AppState -> Widget ResourceName
drawInputBox state =
  withBorderStyle unicode $ borderWithLabel (txt helpText) $
  padAll 1 $ renderEditor (txt . T.unlines) True (state ^. asCurrentEditorText)
  where
    helpText = "Type and press enter to send messge to server.."

drawUI :: AppState -> [Widget ResourceName]
drawUI appState = [drawReadingBox appState <=> drawInputBox appState]

handleEvent :: AppState -> BrickEvent ResourceName WIEvent -> EventM ResourceName (Next AppState)
handleEvent state@AppState{..} ev = case ev of
  -- handle custom events
  AppEvent aev -> case aev of
    WIENewClientMsg m -> continue $ state { _asClientMessages = _asClientMessages ++ [m] }
    WIENewServerMsg m -> continue $ state { _asServerMessages = _asServerMessages ++ [m] }
  -- handle Vty key events
  VtyEvent vev -> case vev of
    Vty.EvKey Vty.KEsc []   -> halt state
    Vty.EvKey Vty.KEnter [] -> handleEnter
    _                       -> handleEventLensed state asCurrentEditorText handleEditorEvent vev >>= continue
  _            -> continue state

  where
    handleEnter = do
      let contents = getEditContents $ state ^. asCurrentEditorText
          newState = state & asCurrentEditorText .~ (resetEdit state)
      continue $ newState { _asClientMessages = _asClientMessages ++ contents }

    -- resetEdit :: AppState -> Editor Text ResourceName
    resetEdit st = applyEdit transformer $ state ^. asCurrentEditorText
    -- transformer :: TextZipper String -> TextZipper String
    transformer _ = textZipper [] (Just 2)

theApp :: App AppState WIEvent ResourceName
theApp = App { appDraw = drawUI
             , appHandleEvent = handleEvent
             , appStartEvent = return
             , appAttrMap = const $ attrMap defAttr []
             , appChooseCursor = showFirstCursor
             }

-- send
runUI :: IO ()
runUI = do
  let initialState = AppState [] [] (editor "edit-box" (Just 2) "")
  chan <- newBChan 100

  void $ forkIO $ forever $ do
    -- t1 <- T.pack . show <$> getCurrentTime
    -- writeBChan chan (WIENewClientMsg $ "new client message " <> t1)
    -- threadDelay $ 1 * 10 ^ 6
    t2 <- T.pack . show <$> getCurrentTime
    writeBChan chan (WIENewServerMsg $ "new server message " <> t2)
    threadDelay $ 3 * 10 ^ 6

  let buildVty = Vty.mkVty Vty.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) theApp initialState
