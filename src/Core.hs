{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Core where

import Codec.Serialise qualified as Serialise
import Data.Aeson qualified as Aeson
import Data.Time.Clock.POSIX qualified as Time
import Docker qualified
import RIO
import RIO.List qualified as List
import RIO.Map qualified as Map
import RIO.NonEmpty as NonEmpty
import RIO.Text as Text hiding (concat)

data Pipeline = Pipeline
  { steps :: NonEmpty Step
  }
  deriving (Eq, Show, Generic, Aeson.FromJSON, Serialise.Serialise)

data Step = Step
  { name :: StepName,
    image :: Docker.Image,
    commands :: NonEmpty Text
  }
  deriving (Eq, Show, Generic, Aeson.FromJSON, Serialise.Serialise)

data Build = Build
  { pipeline :: Pipeline,
    state :: BuildState,
    completedSteps :: Map StepName StepResult,
    volume :: Docker.Volume
  }
  deriving (Eq, Show, Generic, Serialise.Serialise)

data StepResult
  = StepFailed Docker.ContainerExitCode
  | StepSucceeded
  deriving (Eq, Show)

data BuildState
  = BuildReady
  | BuildRunning BuildRunningState
  | BuildFinished BuildResult
  deriving (Eq, Show, Generic, Serialise.Serialise)

data BuildRunningState = BuildRunningState
  { step :: StepName,
    container :: Docker.ContainerId
  }
  deriving (Eq, Show, Generic, Serialise.Serialise)

data BuildResult
  = BuildSucceeded
  | BuildFailed
  | BuildUnexpectedState Text
  deriving (Eq, Show, Generic, Serialise.Serialise)

newtype BuildNumber = BuildNumber Int
  deriving (Eq, Show, Generic, Serialise.Serialise)

buildNumberToInt :: BuildNumber -> Int
buildNumberToInt (BuildNumber n) = n

newtype StepName = StepName Text
  deriving (Eq, Show, Ord, Generic, Aeson.FromJSON, Serialise.Serialise)

stepNameToText :: StepName -> Text
stepNameToText (StepName step) = step

exitCodeToStepResult :: Docker.ContainerExitCode -> StepResult
exitCodeToStepResult exit =
  if Docker.exitCodeToInt exit == 0
    then StepSucceeded
    else StepFailed exit

buildHasNextStep :: Build -> Either BuildResult Step
buildHasNextStep build =
  if allSucceeded
    then case nextStep of
      Just step -> Right step
      Nothing -> Left BuildSucceeded
    else Left BuildFailed
  where
    allSucceeded = List.all (StepSucceeded ==) (completedSteps build)
    nextStep = List.find f (steps . pipeline $ build)
    f step = not $ Map.member step.name (completedSteps build)

progress :: Docker.Service -> Build -> IO Build
progress docker build =
  case build.state of
    BuildReady ->
      case buildHasNextStep build of
        Left result ->
          pure $ build {state = BuildFinished result}
        Right step -> do
          let script =
                Text.unlines $ NonEmpty.toList step.commands
          let options =
                Docker.CreateContainerOptions
                  { image = step.image,
                    script = script,
                    volume = build.volume
                  }

          docker.pullImage step.image
          container <- docker.createContainer options
          docker.startContainer container

          let s =
                BuildRunningState
                  { step = step.name,
                    container = container
                  }
          pure $ build {state = BuildRunning s}
    BuildRunning state -> do
      status <- docker.containerStatus state.container

      case status of
        Docker.ContainerRunning ->
          -- Don't do anything, just wait for it to exit
          pure build
        Docker.ContainerExited exit -> do
          let result = exitCodeToStepResult exit
          pure
            build
              { completedSteps =
                  Map.insert state.step result build.completedSteps,
                state = BuildReady
              }
        Docker.ContainerOther other -> do
          let s = BuildUnexpectedState other
          pure build {state = BuildFinished s}
    BuildFinished _ -> pure build

-- Logs

type LogCollection = Map StepName CollectionStatus

data CollectionStatus
  = CollectionReady
  | CollectingLogs Docker.ContainerId Time.POSIXTime
  | CollectionFinished
  deriving (Eq, Show)

data Log = Log
  { output :: ByteString,
    step :: StepName
  }
  deriving (Eq, Show, Generic, Serialise.Serialise)

initLogCollection :: Pipeline -> LogCollection
initLogCollection pipeline =
  Map.fromList $ NonEmpty.toList steps
  where
    steps = pipeline.steps <&> \step -> (step.name, CollectionReady)

updateCollection ::
  BuildState ->
  Time.POSIXTime ->
  LogCollection ->
  LogCollection
updateCollection state lastCollection =
  Map.mapWithKey f
  where
    update step since nextState =
      case state of
        BuildRunning state ->
          if state.step == step
            then CollectingLogs state.container since
            else nextState
        _ -> nextState

    f step = \case
      CollectionReady ->
        update step 0 CollectionReady
      CollectingLogs _ _ ->
        update step lastCollection CollectionFinished
      CollectionFinished ->
        CollectionFinished

runCollection ::
  Docker.Service ->
  Time.POSIXTime ->
  LogCollection ->
  IO [Log]
runCollection docker collectUntil collection = do
  logs <- Map.traverseWithKey f collection
  pure $ concat (Map.elems logs)
  where
    f step = \case
      CollectionReady -> pure []
      CollectionFinished -> pure []
      CollectingLogs container since -> do
        let options =
              Docker.FetchLogsOptions
                { container = container,
                  since = since,
                  until = collectUntil
                }
        output <- docker.fetchLogs options
        pure [Log {step = step, output = output}]

collectLogs ::
  Docker.Service ->
  LogCollection ->
  Build ->
  IO (LogCollection, [Log])
collectLogs docker collection build = do
  now <- Time.getPOSIXTime
  logs <- runCollection docker now collection
  let newCollection = updateCollection build.state now collection
  pure (newCollection, logs)
