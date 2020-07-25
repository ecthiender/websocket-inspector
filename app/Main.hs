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

import           TUI
import           Websocket

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
