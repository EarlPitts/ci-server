{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent where

import Codec.Serialise qualified as Serialise
import Core
import Network.HTTP.Simple qualified as HTTP
import RIO
import Runner qualified

data Cmd
  = StartBuild BuildNumber Pipeline
  deriving (Eq, Show, Generic, Serialise.Serialise)

data Msg
  = LogCollected BuildNumber Log
  | BuildUpdated BuildNumber Build
  deriving (Eq, Show, Generic, Serialise.Serialise)

data Config = Config
  { endpoint :: String
  }

runCommand :: Runner.Service -> Cmd -> IO ()
runCommand runner = \case
  StartBuild number pipeline -> do
    let hooks =
          Runner.Hooks
            { logCollected = traceShowIO -- TODO
            }

    build <- runner.prepareBuild pipeline
    void $ runner.runBuild hooks build

run :: Config -> Runner.Service -> IO ()
run config runner = forever do
  endpoint <- HTTP.parseRequest config.endpoint
  let req =
        endpoint
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestPath "/agent/pull"

  res <- HTTP.httpLBS req
  let cmd = Serialise.deserialise (HTTP.getResponseBody res) :: Maybe Cmd

  traverse_ (runCommand runner) cmd

  threadDelay (1 * 1000 * 1000)
