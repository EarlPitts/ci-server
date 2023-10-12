{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import Core
import JobHandler qualified
import RIO
import Web.Scotty qualified as Scotty
import qualified Codec.Serialise as Serialise

data Config = Config
  { port :: Int
  }

run :: Config -> JobHandler.Service -> IO ()
run config handler =
  Scotty.scotty config.port do
    Scotty.post "/agent/pull" do
      cmd <- Scotty.liftAndCatchIO do
        handler.dispatchCmd

      Scotty.raw $ Serialise.serialise cmd
