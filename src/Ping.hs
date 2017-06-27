module Ping where

import Data.Word (Word16)
import Data.Binary.Put (Put, putWord8, putWord16be, putWord32be, runPut)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Network.Socket (Family(AF_INET),Socket, SocketType(Raw), SockAddr(SockAddrInet),addrAddress,addrFamily,addrProtocol,addrSocketType, ProtocolNumber, connect,isConnected, getAddrInfo, socket, close)
import Network.Socket.ByteString (send, recv)
import System.Posix.Process (getProcessID)

icmpProtocol :: ProtocolNumber
icmpProtocol = 1

printConnected :: Socket -> IO()
printConnected s = do
                    c <- isConnected s
                    putStrLn $ "Connected to socket: " ++ (show c)

icmpRequest :: Word16 -> Put
icmpRequest pid = do
  putWord8 8 -- type
  putWord8 0 -- code
  putWord16be 0 -- ICMP Header Checksum, big endian
  putWord16be pid -- Identifier (PID) - big endian
  putWord16be 1 -- Sequence Number - big endian
  putWord32be 0 -- Data

helloWorld :: IO()
helloWorld = do
  pid <- getProcessID
  _ <- putStrLn $ "------------------ Starting haskell ping service with Process ID " ++ (show pid) ++ " ------------------"
  sock <- socket AF_INET Raw icmpProtocol
  addrInfo <- getAddrInfo Nothing (Just "127.0.0.1") Nothing -- Don't need to provide hints, since only host matters.
  let sockAddress = addrAddress $ head addrInfo
  connected <- connect sock (sockAddress)
  _ <- printConnected sock
  _ <- putStrLn $ "Socket Address: " ++ (show $ sockAddress)
  bytesSent <- send sock $ (B.concat . BL.toChunks . runPut) $ icmpRequest (fromIntegral pid)
  _ <- putStrLn $ "Number of Bytes Sent: " ++ (show bytesSent)
  close sock -- We're done here, close the socket
  _ <- printConnected sock
  putStrLn "goodbye"
