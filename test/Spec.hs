{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Agent qualified
import Control.Concurrent.Async qualified as Async
import Core
import Data.Yaml qualified as Yaml
import Docker qualified
import JobHandler qualified
import RIO
import RIO.ByteString as ByteString
import RIO.Map as Map
import RIO.NonEmpty.Partial qualified as NonEmpty.Partial
import RIO.Set qualified as Set
import Runner qualified
import Server qualified
import System.Process.Typed qualified as Process
import Test.Hspec

-- Helper functions
makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands =
  Step
    { name = StepName name,
      image = Docker.Image {name = image, tag = "latest"},
      commands = NonEmpty.Partial.fromList commands
    }

makePipeline :: [Step] -> Pipeline
makePipeline steps =
  Pipeline {steps = NonEmpty.Partial.fromList steps}

-- Test values
testPipeline :: Pipeline
testPipeline =
  makePipeline
    [ makeStep "First step" "alpine" ["date"],
      makeStep "Second step" "alpine" ["uname -r"]
    ]

emptyHooks :: Runner.Hooks
emptyHooks =
  Runner.Hooks
    { logCollected = const $ pure ()
    }

testRunSuccess :: Runner.Service -> IO ()
testRunSuccess runner = do
  build <-
    runner.prepareBuild $
      makePipeline
        [ makeStep "First step" "alpine" ["date"],
          makeStep "Second step" "alpine" ["uname -r"]
        ]
  result <- runner.runBuild emptyHooks build

  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps
    `shouldBe` [StepSucceeded, StepSucceeded]

testRunFailure :: Runner.Service -> IO ()
testRunFailure runner = do
  build <-
    runner.prepareBuild $
      makePipeline
        [ makeStep "Should fail" "alpine" ["exit 1"]
        ]
  result <- runner.runBuild emptyHooks build

  result.state `shouldBe` BuildFinished BuildFailed
  Map.elems result.completedSteps
    `shouldBe` [StepFailed (Docker.ContainerExitCode 1)]

testSharedWorkspace :: Runner.Service -> IO ()
testSharedWorkspace runner = do
  build <-
    runner.prepareBuild $
      makePipeline
        [ makeStep "Create file" "alpine" ["touch test"],
          makeStep "Check file" "alpine" ["test -e test"]
        ]

  result <- runner.runBuild emptyHooks build
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

testLogCollection :: Runner.Service -> IO ()
testLogCollection runner = do
  expected <- newMVar (Set.fromList ["hello", "world", "Linux"])

  let onLog :: Log -> IO ()
      onLog log = do
        remaining <- readMVar expected
        forM_ remaining $ \word ->
          if ByteString.isInfixOf word log.output
            then pure () -- Not found
            else modifyMVar_ expected (pure . Set.delete word)

  let hooks = Runner.Hooks {logCollected = onLog}

  build <-
    runner.prepareBuild $
      makePipeline
        [ makeStep "Long step" "alpine" ["echo hello", "sleep 2", "echo world"],
          makeStep "Echo Linux" "alpine" ["uname -s"]
        ]
  result <- runner.runBuild hooks build
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

  logs <- readMVar expected
  logs `shouldBe` Set.empty

testImagePull :: Runner.Service -> IO ()
testImagePull runner = do
  Process.readProcessStdout "docker rmi -f busybox"

  build <-
    runner.prepareBuild $
      makePipeline
        [ makeStep "First step" "busybox" ["date"]
        ]
  result <- runner.runBuild emptyHooks build

  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded]

testYamlDecoding :: Runner.Service -> IO ()
testYamlDecoding runner = do
  pipeline <- Yaml.decodeFileThrow "test/pipeline.sample.yml"
  build <- runner.prepareBuild pipeline
  result <- runner.runBuild emptyHooks build
  result.state `shouldBe` BuildFinished BuildSucceeded

checkBuild :: JobHandler.Service -> BuildNumber -> IO ()
checkBuild handler number = loop
  where
    loop = do
      Just job <- handler.findJob number
      case job.state of
        JobHandler.JobScheduled build -> do
          case build.state of
            BuildFinished s -> s `shouldBe` BuildSucceeded
            _ -> loop
        _ -> loop

testServerAndAgent :: Runner.Service -> IO ()
testServerAndAgent runner = do
  let handler = undefined :: JobHandler.Service -- TODO
  serverThread <- Async.async do
    Server.run (Server.Config 9000) handler

  Async.link serverThread

  agentThread <- Async.async do
    Agent.run (Agent.Config "http://localhost:9000") runner

  Async.link agentThread

  let pipeline =
        makePipeline
          [ makeStep "agent-test" "busybox" ["echo hello", "echo from agent"]
          ]

  number <- handler.queueJob pipeline
  checkBuild handler number

  Async.cancel serverThread
  Async.cancel agentThread

  pure ()

cleanupDocker :: IO ()
cleanupDocker = void do
  Process.readProcessStdout "docker rm -f $(docker ps -aq --filter \"label=quad\")"
  Process.readProcessStdout "docker volume rm -f $(docker volume ls -q --filter \"label=quad\")"

main :: IO ()
main = hspec do
  docker <- runIO Docker.createService
  runner <- runIO $ Runner.createService docker
  beforeAll cleanupDocker $ describe "ci-server" do
    it "should run a build (success)" do
      testRunSuccess runner
    it "should run a build (failure)" do
      testRunFailure runner
    it "should share a workspace between steps" do
      testSharedWorkspace runner
    it "should collect logs" do
      testLogCollection runner
    it "should pull images" do
      testImagePull runner
    it "should decode pipelines" do
      testYamlDecoding runner
    it "should run server and agent" do
      testServerAndAgent runner
