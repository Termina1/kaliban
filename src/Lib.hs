module Lib where

import VK.API
import VK.ResponseTypes
import VK.API.Messages
import VK.Longpoll
import Data.Optional
import Conduits.VK
import Conduit
import Control.Concurrent.Chan
import Brain

botLoop :: ConduitChannel -> IO ()
botLoop chan = do (event, pushback) <- readChan chan
                  response <- processRequest event
                  writeChan pushback response
                  botLoop chan

startBot :: [ConduitInstance] -> IO ()
startBot instances = do chan <- startConduits instances
                        botLoop chan
                        return ()