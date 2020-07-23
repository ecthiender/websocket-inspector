{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Brick
import           Brick.BChan
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad      (forever, void)
import           Data.Time.Clock
import           Lib
import           Options.Generic    (Generic)
import           System.Environment (getArgs)

import qualified Data.Text          as T
import qualified Network.WebSockets as WS
import qualified Options.Generic    as Options

data AppOptions
  = AppOptions
  { --url :: String Options.<?> "URL Format: ws://example.com:8080/some_path"
    host :: String
  , port :: Int
  , path :: String
  } deriving (Show, Generic)

instance Options.ParseRecord AppOptions

main :: IO ()
main = do
  AppOptions{..} <- Options.getRecord "websocket-inspector"
  brickChan <- newBChan 100
  clientChan <- newBChan 100

  forkIO $ WS.runClient host port path $ websocketClient brickChan clientChan
  runUI brickChan clientChan

  -- WS.runSecureClient "gateway.discord.gg" 443 "/?v=6&encoding=json" $ \conn -> do

websocketClient :: BChan WIEvent -> BChan Options.Text -> WS.Connection -> IO b
websocketClient brickChan clientChan conn = do
  void $ forkIO $ forever $ do
    msg <- readBChan clientChan
    curTime <- getCurrentTime
    writeBChan brickChan (WIENewClientMsg msg curTime)
    WS.sendTextData conn msg

  forever $ do
    msg <- WS.receiveData conn
    curTime <- getCurrentTime
    writeBChan brickChan (WIENewServerMsg msg curTime)
