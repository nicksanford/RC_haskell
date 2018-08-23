-- Echo server program
module Echo (run) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main = withSocketDo $ do
  addr <- resolve "3000"
  E.bracket (open addr) close loop
  where
    resolve port = do
      let hints = defaultHints {
              addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }
      addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
      print addr
      return addr
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      print sock
      setSocketOption sock ReuseAddr 1
      bind sock (addrAddress addr)
      -- if the prefork technique is not used,
      -- set CloseOnExec for the security reasons.
      let fd = fdSocket sock
      setCloseOnExecIfNeeded fd
      listen sock 10
      return sock
    loop sock = forever $ do
      (conn, peer) <- accept sock
      putStrLn $ "Connection from " ++ show peer
      void $ forkFinally (talk conn) (\_ -> close conn)
    talk conn = do
      msg <- recv conn 1024
      unless (S.null msg) $ do
        sendAll conn msg
        talk conn
