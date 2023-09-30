module Socket where

import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.Internal qualified as Client.Internal
import Network.Socket qualified as S
import Network.Socket.ByteString qualified as SBS
import RIO

newManager :: FilePath -> IO Client.Manager
newManager fp =
  Client.newManager $
    Client.defaultManagerSettings
      { Client.managerRawConnection = pure makeSocket
      }
  where
    makeSocket _ _ _ = do
      s <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
      S.connect s (S.SockAddrUnix fp)
      Client.Internal.makeConnection
        (SBS.recv s 8096)
        (SBS.sendAll s)
        (S.close s)
