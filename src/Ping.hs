module Ping where

import Data.Bits
import Data.Word (Word8, Word16, Word32)
import Data.Binary.Get (Get, getWord16be, isEmpty, runGet)
import Data.Binary.Put (Put, putWord8, putWord16be, putLazyByteString, runPut)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Network.Socket (Family(AF_INET), Socket, SocketType(Raw), SockAddr(SockAddrInet),addrAddress,addrFamily, addrProtocol, addrSocketType, ProtocolNumber, connect,isConnected, getAddrInfo, socket, close)
import Network.Socket.ByteString (sendTo, recvFrom)
import System.Posix.Process (getProcessID)

-- to run: >$ stack build && sudo stack exec hsping --allow-different-user

newtype PID = PID Word16
newtype Sequence = Sequence Word16
type PacketsSent = Int
type PacketsReceived = Int

icmpData :: BL.ByteString
icmpData = let numBytes = 7
           in BL.pack [1..(8*numBytes)]
           
maxReceive = 2048 -- maximum number of bits to receive
ipHeaderLength = 20 -- length of IP address header in bytes
icmpProtocol :: ProtocolNumber
icmpProtocol = 1

-- Build type and data constructors for a strongly typed request
data ICMPRequest = ICMPRequest {
    _type :: Word8
  , _code :: Word8
  , _checksum :: Word16
  , _identifier :: Word16
  , _sequence :: Word16
  , _data :: BL.ByteString
}

-- getICMPHeader :: Get (Word8, Word8, Word16, Word16, Word16)
-- getICMPHeader = do


writeToBuffer :: ICMPRequest -> Put
writeToBuffer icmp = do
  putWord8 $ _type icmp
  putWord8 $ _code icmp
  putWord16be $ _checksum icmp
  putWord16be $ _identifier icmp
  putWord16be $ _sequence icmp
  putLazyByteString $ _data icmp

buildRequest :: PID -> Sequence -> BL.ByteString -> ICMPRequest
buildRequest (PID pid) (Sequence seq) icmpdata = ICMPRequest 8 0 checksum pid seq icmpdata
  where
    initialBuild :: ICMPRequest
    initialBuild = ICMPRequest 8 0 0 pid 1 icmpdata

    -- "If the total length is odd, the received data is padded with one
    -- octet of zeros for computing the checksum." - RFC 792
    maybeAddOctet :: BL.ByteString -> BL.ByteString
    maybeAddOctet bs
            | (BL.length bs) `mod` 2 == 0 = bs
            | otherwise                   = BL.snoc bs 0  -- pad with an octet of zeros

    --  Split the ICMP header + payload (data) into 16 bit words
    splitBuffer :: Get [Word16]
    splitBuffer = do
      empty <- isEmpty
      if empty then return []
        else do 
          w16 <- getWord16be
          rest <- splitBuffer
          return (w16 : rest)

    values :: (Num a) => [a]
    values = map fromIntegral $ runGet splitBuffer ((maybeAddOctet . runPut . writeToBuffer) initialBuild)

    total :: Word32
    total = sum values -- 32 bit sum

    low = fromIntegral $ total :: Word16  -- sum without carries
    high = fromIntegral $ (total `shiftR` 16) :: Word16 -- sum of the carries
    eac = low + high -- calculate end around carry

    checksum :: Word16
    checksum = complement $ eac

pingHost :: Socket -> SockAddr -> PID -> Sequence -> IO()
pingHost s sa pid seq = do
  bytesSent <- sendTo s ((B.concat . BL.toChunks . runPut . writeToBuffer) $ buildRequest pid seq icmpData) sa
  -- (response, senderAddress) <- recvFrom s maxReceive
  putStrLn $ "Number of Bytes Sent: " ++ (show bytesSent)
  

ping :: IO()
ping = do
  pid <- getProcessID
  _ <- putStrLn $ "--- Starting haskell ping service with Process ID " ++ (show pid) ++ " ---"
  sock <- socket AF_INET Raw icmpProtocol
  addrInfo <- getAddrInfo Nothing (Just "127.0.0.1") Nothing -- Don't need to provide hints, since only host matters.
  let sockAddress = addrAddress $ head addrInfo
  _ <- putStrLn $ "Data: " ++ (show $ icmpData)
  _ <- putStrLn $ "Socket Address: " ++ (show $ sockAddress)
  pingHost sock sockAddress (PID (fromIntegral pid)) (Sequence 0)
  putStrLn "goodbye"
