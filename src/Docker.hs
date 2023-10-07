{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Docker where

import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types as Aeson.Types
import Network.HTTP.Simple qualified as HTTP
import RIO
import Socket qualified

newtype Image = Image Text
  deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode Int
  deriving (Eq, Show)

data ContainerStatus
  = ContainerRunning
  | ContainerExited ContainerExitCode
  | ContainerOther Text
  deriving (Eq, Show)

type RequestBuilder = Text -> HTTP.Request

imageToText :: Image -> Text
imageToText (Image img) = img

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

data CreateContainerOptions = CreateContainerOptions
  { image :: Image,
    script :: Text
  }

newtype ContainerId = ContainerId Text
  deriving (Eq, Show)

data Service = Service
  { createContainer :: CreateContainerOptions -> IO ContainerId,
    startContainer :: ContainerId -> IO (),
    containerStatus :: ContainerId -> IO ContainerStatus
  }

createService :: IO Service
createService = do
  manager <- Socket.newManager "/var/run/docker.sock"

  let makeReq :: RequestBuilder
      makeReq path =
        HTTP.defaultRequest
          & HTTP.setRequestPath (encodeUtf8 $ "/v1.40" <> path)
          & HTTP.setRequestManager manager
  pure
    Service
      { createContainer = createContainer_ makeReq,
        startContainer = startContainer_ makeReq,
        containerStatus = containerStatus_ makeReq
      }

containerIdToText :: ContainerId -> Text
containerIdToText (ContainerId c) = c

createContainer_ :: RequestBuilder -> CreateContainerOptions -> IO ContainerId
createContainer_ makeReq options = do
  let img = imageToText options.image
  let body =
        Aeson.object
          [ ("Image", Aeson.toJSON img),
            ("Tty", Aeson.toJSON True),
            ("Labels", Aeson.object [("quad", "")]),
            ("Cmd", "echo \"$QUAD_SCRIPT\" | /bin/sh"),
            ("Env", Aeson.toJSON ["QUAD_SCRIPT=" <> options.script]),
            ("Entrypoint", Aeson.toJSON [Aeson.String "/bin/sh", "-c"])
          ]
  let parser = Aeson.withObject "create-container" $ \o -> do
        cId <- o .: "Id"
        pure $ ContainerId cId

  let req =
        makeReq "/containers/create"
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestBodyJSON body

  res <- HTTP.httpBS req
  parseResponse res parser

parseResponse ::
  HTTP.Response ByteString ->
  (Aeson.Value -> Aeson.Types.Parser a) ->
  IO a
parseResponse res parser = do
  let result = do
        value <- Aeson.eitherDecodeStrict (HTTP.getResponseBody res)
        Aeson.Types.parseEither parser value
  case result of
    Left e -> throwString e
    Right status -> pure status

containerStatus_ :: RequestBuilder -> ContainerId -> IO ContainerStatus
containerStatus_ makeReq container = do
  let parser = Aeson.withObject "container-inspect" $ \o -> do
        state <- o .: "State"
        status <- state .: "Status"
        case status of
          "running" -> pure ContainerRunning
          "exited" -> do
            code <- state .: "ExitCode"
            pure $ ContainerExited (ContainerExitCode code)
          other -> pure $ ContainerOther other

  let req =
        makeReq $
          "/containers/" <> containerIdToText container <> "/json"

  res <- HTTP.httpBS req
  parseResponse res parser

startContainer_ :: RequestBuilder -> ContainerId -> IO ()
startContainer_ makeReq container = do
  let path =
        "/containers/"
          <> containerIdToText container
          <> "/start"

  let req =
        makeReq path
          & HTTP.setRequestMethod "POST"

  void $ HTTP.httpBS req
