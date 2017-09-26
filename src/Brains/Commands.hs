{-# LANGUAGE GADTs, StandaloneDeriving #-}

module Brains.Commands where

import Data.Optional
import Data.Time

data BrainCommand where
  UnknownCommand :: BrainCommand
  CreateTaskCommand
    :: { description :: String
       , duedate :: Optional LocalTime}
    -> BrainCommand
