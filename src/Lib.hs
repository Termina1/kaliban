module Lib where

import Brain
import Conduit
import Conduits.VK
import Control.Concurrent.Chan
import Control.Concurrent (forkIO)
import Data.Optional
import VK.API.Messages
import VK.API
import VK.Longpoll
import VK.ResponseTypes

botLoop :: ConduitChannel -> IO ()
botLoop chan = do
  (event, pushback) <- readChan chan
  forkIO (do response <- processRequest testCells event
             writeChan pushback response)
  botLoop chan

startBot :: [ConduitInstance] -> IO ()
startBot instances = do
  chan <- startConduits instances
  botLoop chan
  return ()
