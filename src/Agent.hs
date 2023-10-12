{-# LANGUAGE DeriveAnyClass #-}

module Agent where

import Codec.Serialise qualified as Serialise
import Core
import RIO

data Cmd
  = StartBuild BuildNumber Pipeline
  deriving (Eq, Show, Generic, Serialise.Serialise)

data Msg
  = LogCollected BuildNumber Log
  | BuildUpdated BuildNumber Build
  deriving (Eq, Show, Generic, Serialise.Serialise)
