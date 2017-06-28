module Ping where

import Data.Bits
import Data.Word (Word8, Word16, Word32)
import Data.Binary.Put (Put, putWord8, putWord16be, putLazyByteString, runPut)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Network.Socket (Family(AF_INET), Socket, SocketType(Raw), SockAddr(SockAddrInet),addrAddress,addrFamily, addrProtocol, addrSocketType, ProtocolNumber, connect,isConnected, getAddrInfo, socket, close)
import Network.Socket.ByteString (send, recv)
import System.Posix.Process (getProcessID)

-- to run: >$ stack build && sudo stack exec hsping --allow-different-user

icmpData :: BL.ByteString
icmpData = let numBytes = 7
           in BL.pack [1..(8*numBytes)]

icmpProtocol :: ProtocolNumber
icmpProtocol = 1

printConnected :: Socket -> IO()
printConnected s = do
                    c <- isConnected s
                    putStrLn $ "Connected to socket: " ++ (show c)

-- Build type and data constructors for a strongly typed request
data ICMPRequest = ICMPRequest {
    _type :: Word8
  , _code :: Word8
  , _checksum :: Word16
  , _identifier :: Word16
  , _sequence :: Word16
  , _data :: BL.ByteString
}

buildRequest :: Word16 -> BL.ByteString -> ICMPRequest
buildRequest pid lbs = ICMPRequest 8 0 0 pid 1 lbs

icmpRequest :: ICMPRequest -> Put
icmpRequest icmp = do
  putWord8 $ _type icmp
  putWord8 $ _code icmp
  putWord16be $ _checksum icmp
  putWord16be $ _identifier icmp
  putWord16be $ _sequence icmp
  putLazyByteString $ _data icmp

helloWorld :: IO()
helloWorld = do
  pid <- getProcessID
  _ <- putStrLn $ "-------------- Starting haskell ping service with Process ID " ++ (show pid) ++ " --------------"
  sock <- socket AF_INET Raw icmpProtocol
  addrInfo <- getAddrInfo Nothing (Just "127.0.0.1") Nothing -- Don't need to provide hints, since only host matters.
  let sockAddress = addrAddress $ head addrInfo
  connected <- connect sock (sockAddress)
  _ <- printConnected sock
  _ <- putStrLn $ "Data: " ++ (show $ icmpData)
  _ <- putStrLn $ "Socket Address: " ++ (show $ sockAddress)
  bytesSent <- send sock $ (B.concat . BL.toChunks . runPut . icmpRequest) $ buildRequest (fromIntegral pid) icmpData
  _ <- putStrLn $ "Number of Bytes Sent: " ++ (show bytesSent)
  close sock -- We're done here, close the socket
  _ <- printConnected sock
  putStrLn "goodbye"
