{-# LANGUAGE OverloadedRecordDot #-}

module Runner where

import Core
import Docker qualified
import RIO

data Service = Service
  { runBuild :: Hooks -> Build -> IO Build,
    prepareBuild :: Pipeline -> IO Build
  }

data Hooks = Hooks
  { logCollected :: Log -> IO ()
  }

createService :: Docker.Service -> IO Service
createService docker = do
  pure
    Service
      { runBuild = runBuild_ docker,
        prepareBuild = prepareBuild_ docker
      }

prepareBuild_ :: Docker.Service -> Pipeline -> IO Build
prepareBuild_ docker pipeline = do
  volume <- docker.createVolume
  pure
    Build
      { pipeline = pipeline,
        state = BuildReady,
        completedSteps = mempty,
        volume = volume
      }

runBuild_ :: Docker.Service -> Hooks -> Build -> IO Build
runBuild_ docker hooks build = do
  newBuild <- Core.progress docker build
  case newBuild.state of
    BuildFinished _ ->
      pure newBuild
    _ -> do
      threadDelay (1 * 1000 * 1000)
      runBuild_ docker hooks newBuild
