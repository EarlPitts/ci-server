{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Core
import Docker qualified
import RIO
import RIO.Map as Map
import RIO.NonEmpty.Partial qualified as NonEmpty.Partial
import Runner qualified
import System.Process.Typed qualified as Process
import Test.Hspec

-- Helper functions
makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands =
  Step
    { name = StepName name,
      image = Docker.Image image,
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

testBuild :: Build
testBuild =
  Build
    { pipeline = testPipeline,
      state = BuildReady,
      completedSteps = mempty
    }

testRunSuccess :: Runner.Service -> IO ()
testRunSuccess runner = do
  build <-
    runner.prepareBuild $
      makePipeline
        [ makeStep "First step" "alpine" ["date"],
          makeStep "Second step" "alpine" ["uname -r"]
        ]
  result <- runner.runBuild build

  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps
    `shouldBe` [StepSucceeded, StepSucceeded]

testRunFailure :: Runner.Service -> IO ()
testRunFailure runner = do
  build <- runner.prepareBuild $ makePipeline
              [ makeStep "Should fail" "alpine" ["exit 1"]
              ]
  result <- runner.runBuild build
  
  result.state `shouldBe` BuildFinished BuildFailed
  Map.elems result.completedSteps
    `shouldBe` [StepFailed (Docker.ContainerExitCode 1)]

cleanupDocker :: IO ()
cleanupDocker = void do
  Process.runProcess "docker rm -f $(docker ps -aq --filter \"label=quad\")"

main :: IO ()
main = hspec do
  docker <- runIO Docker.createService
  runner <- runIO $ Runner.createService docker
  beforeAll cleanupDocker $ describe "ci-server" do
    it "should run a build (success)" do
      testRunSuccess runner
    it "should run a build (failure)" do
      testRunFailure runner
