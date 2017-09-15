module Lib where

import Brain
import Conduit
import Conduits.VK
import Control.Concurrent.Chan
import Data.Optional
import VK.API
import VK.API.Messages
import VK.Longpoll
import VK.ResponseTypes

botLoop :: ConduitChannel -> IO ()
botLoop chan = do
  (event, pushback) <- readChan chan
  response <- processRequest event
  writeChan pushback response
  botLoop chan

startBot :: [ConduitInstance] -> IO ()
startBot instances = do
  chan <- startConduits instances
  botLoop chan
  return ()
