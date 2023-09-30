{-# LANGUAGE OverloadedStrings #-}

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

imageToText :: Image -> Text
imageToText (Image img) = img

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

data CreateContainerOptions = CreateContainerOptions
  { image :: Image
  }

newtype ContainerId = ContainerId Text
  deriving (Eq, Show)

containerIdToText :: ContainerId -> Text
containerIdToText (ContainerId c) = c

createContainer :: CreateContainerOptions -> IO ContainerId
createContainer options = do
  manager <- Socket.newManager "/var/run/docker.sock"

  let img = imageToText (image options)
  let body =
        Aeson.object
          [ ("Image", Aeson.toJSON img),
            ("Tty", Aeson.toJSON True),
            ("Labels", Aeson.object [("quad", "")]),
            ("Cmd", "echo hello"),
            ("Entrypoint", Aeson.toJSON [Aeson.String "/bin/sh", "-c"])
          ]
  let parser = Aeson.withObject "create-container" $ \o -> do
        cId <- o .: "Id"
        pure $ ContainerId cId

  let req =
        HTTP.defaultRequest
          & HTTP.setRequestManager manager
          & HTTP.setRequestPath "/v1.40/containers/create"
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

startContainer :: ContainerId -> IO ()
startContainer container = do
  manager <- Socket.newManager "/var/run/docker.sock"

  let path = "/v1.40/containers/" <>
             containerIdToText container <>
             "/start"

  let req = HTTP.defaultRequest
          & HTTP.setRequestManager manager
          & HTTP.setRequestPath (encodeUtf8 path)
          & HTTP.setRequestMethod "POST"

  traceShowIO req

  void $ HTTP.httpBS req
