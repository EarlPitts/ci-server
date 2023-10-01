{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE FlexibleContexts #-}

module Core where

import Docker qualified
import RIO
import RIO.List qualified as List
import RIO.Map qualified as Map

data Pipeline = Pipeline
  { steps :: NonEmpty Step
  }
  deriving (Eq, Show)

data Step = Step
  { name :: StepName,
    image :: Docker.Image,
    commands :: NonEmpty Text
  }
  deriving (Eq, Show)

data Build = Build
  { pipeline :: Pipeline,
    state :: BuildState,
    completedSteps :: Map StepName StepResult
  }
  deriving (Eq, Show)

data StepResult
  = StepFailed Docker.ContainerExitCode
  | StepSucceeded
  deriving (Eq, Show)

data BuildState
  = BuildReady
  | BuildRunning BuildRunningState
  | BuildFinished BuildResult
  deriving (Eq, Show)

data BuildRunningState = BuildRunningState
  { step :: StepName
  }
  deriving (Eq, Show)

data BuildResult
  = BuildSucceeded
  | BuildFailed
  deriving (Eq, Show)

newtype StepName = StepName Text
  deriving (Eq, Show, Ord)

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
    allSucceeded = List.all ((==) StepSucceeded) (completedSteps build)
    nextStep = List.find f (steps . pipeline $ build)
    f step = not $ Map.member step.name (completedSteps build)

progress :: Docker.Service -> Build -> IO Build
progress docker build =
  case state build of
    BuildReady ->
      case buildHasNextStep build of
        Left result ->
          pure $ build {state = BuildFinished result}
        Right step -> do
          let options = Docker.CreateContainerOptions (image step)
          container <- docker.createContainer options
          docker.startContainer container

          let s = BuildRunningState {step = name step}
          pure $ build {state = BuildRunning s}
    BuildRunning state -> do
      let exit = Docker.ContainerExitCode 0
          result = exitCodeToStepResult exit
      pure
        build
          { state = BuildReady,
            completedSteps =
              Map.insert (step state) result (completedSteps build)
          }
    BuildFinished _ -> pure build
