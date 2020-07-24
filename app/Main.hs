{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import           Brick
import           Brick.BChan
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Exception  (IOException, SomeException, catch, try)
import           Control.Monad      (forever, void)
import           Data.Maybe         (fromMaybe)
import           Data.Time.Clock
import           Network.URI
import           Options.Generic    (Generic)
import           System.Environment (getArgs)
import           Text.Read          (readMaybe)

import qualified Data.Text          as T
import qualified Network.WebSockets as WS
import qualified Options.Generic    as Options
import qualified Wuss               as WS

import           Lib

data AppOptions
  = AppOptions
  { url :: String
  } deriving (Show, Generic)

instance Options.ParseRecord AppOptions

main :: IO ()
main = do
  options <- Options.getRecord "websocket-inspector"
  (scheme, host, port, path) <- either error return $ parseURL $ url options

  brickChan <- newBChan 100
  clientChan <- newBChan 100

  -- Start the websocket system in a separate thread; as the main thread will run the UI
  forkIO $
    runWSClient scheme host port path (websocketClient brickChan clientChan)
      `catch` (writeBChan brickChan . WIEConnectionUpdate . CSError . T.pack . show @IOException)

  -- run the UI on the main thread
  runUI brickChan clientChan
  where
    runWSClient scheme host port path app = case scheme of
      "ws:"  -> WS.runClient host port path app
      "wss:" -> WS.runSecureClient host (fromIntegral port) path app

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

websocketClient :: BChan WIEvent -> BChan Options.Text -> WS.Connection -> IO b
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
