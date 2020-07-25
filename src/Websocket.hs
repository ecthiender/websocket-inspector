{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Websocket where

import           Brick.BChan
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Exception  (IOException, SomeException, catch, try)
import           Control.Monad      (forever, void)
import           Data.Maybe         (fromMaybe)
import           Data.Text          (Text)
import           Data.Time.Clock
import           Network.URI
import           Text.Read          (readMaybe)

import qualified Network.WebSockets as WS
import qualified Wuss               as WS

import           TUI

runWSClient :: String -> String -> Int -> String -> WS.ClientApp a -> IO a
runWSClient scheme host port path app = case scheme of
  "ws:"  -> WS.runClient host port path app
  "wss:" -> WS.runSecureClient host (fromIntegral port) path app

websocketClient :: BChan WIEvent -> BChan Text -> WS.Connection -> IO b
websocketClient brickChan clientChan conn = do
  writeBChan brickChan (WIEConnectionUpdate CSConnected)

  void $ forkIO $ forever $ do
    msg <- readBChan clientChan
    curTime <- getCurrentTime
    writeBChan brickChan (WIENewClientMsg msg curTime)
    WS.sendTextData conn msg

  forever $ do
    eitherMsg <- try @IOException $ WS.receiveData conn
    curTime <- getCurrentTime
    case eitherMsg of
      Left e    -> writeBChan brickChan (WIEConnectionUpdate CSClosed)
      Right msg -> writeBChan brickChan (WIENewServerMsg msg curTime)

parseURL :: String -> Either String (String, String, Int, String)
parseURL url' =
  case parseURI url' of
    Nothing -> Left "Invalid URI. Format: ws://example.com:8080/somepath"
    Just URI{..} ->
      let host = maybe (Left "Invalid hostname") (Right . uriRegName) uriAuthority
          port = fromMaybe (defaultPort uriScheme) $ readMaybe =<< fmap (drop 1 . uriPort) uriAuthority
          path = uriPath <> uriQuery <> uriFragment
      in (,,,) <$> pure uriScheme <*> host <*> pure port <*> pure path
  where
    defaultPort scheme = case scheme of
      "ws:"  -> 80
      "wss:" -> 443

