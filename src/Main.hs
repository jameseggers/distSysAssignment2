{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Network.Socket hiding (send, sendTo, recv, recvFrom)
-- import Network.Socket.ByteString (send, recv)
-- import qualified Data.ByteString.Char8 as B8
import Network.Socket
import Data.Text hiding (head, tail, splitOn)
import Data.List.Split

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

runServer :: (Socket, SockAddr) -> IO Int
runServer (sock, addr) = do
  message <- recv sock 4096
  let stripedMessage = strip $ pack message
  let response = respondToMessage sock addr stripedMessage
  send sock response

respondToMessage :: Socket -> SockAddr -> Text -> String
respondToMessage sock addr "HELO text" = do
  unpack "HELO text\nIP:"++justAddress++"\nPort:"++justPort++"\nStudentID:13330379\n"
  where address = (show addr)
        splitedAddress = splitOn ":" address
        justAddress = head splitedAddress
        justPort = splitedAddress !! 1
respondToMessage sock addr "KILL_SERVICE" = "die"
