{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Brick
import           Lib
import           Options.Generic    (Generic)
import           System.Environment (getArgs)

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
  runUI
  -- WS.runClient host port path websocketClient
  -- WS.runSecureClient "gateway.discord.gg" 443 "/?v=6&encoding=json" $ \conn -> do

-- websocketClient :: WS.Connection -> IO ()
-- websocketClient conn = do
--   logDebug "Connected!"
--   forever $ do
--     msg <- liftIO $ WS.receiveData conn
--     liftIO $ Chan.writeChan eventStream msg
