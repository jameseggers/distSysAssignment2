{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Network.Socket hiding (send, sendTo, recv, recvFrom)
-- import Network.Socket.ByteString (send, recv)
-- import qualified Data.ByteString.Char8 as B8
import Network.Socket
import Data.Text

main :: IO ()
main = withSocketsDo $ do
  sock <- socket socketFamily socketType defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock address
  listen sock 2
  waitForConnection sock
  where socketType = Stream
        socketFamily = AF_INET
        address = SockAddrInet 4243 iNADDR_ANY

waitForConnection :: Socket -> IO ()
waitForConnection sock = do
  conn <- accept sock
  runServer conn
  waitForConnection sock

runServer :: (Socket, SockAddr) -> IO ()
runServer (sock, _) = do
  message <- recv sock 4096
  let stripedMessage = strip $ pack message
  if stripedMessage == "HELLO" then
    putStrLn "LOL"
  else
    putStrLn "omg"
