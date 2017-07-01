module Ping where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Data.Bits
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Binary.Get (Get, getWord8, getWord16be, getWord32be, getWord64be, isEmpty, runGet)
import Data.Binary.Put (Put, putWord8, putWord16be, putWord64be, putLazyByteString, runPut)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Network.Socket (Family(AF_INET), Socket, SocketType(Raw), SockAddr(SockAddrInet),addrAddress,addrFamily, addrProtocol, addrSocketType, ProtocolNumber, connect,isConnected, getAddrInfo, socket, close)
import Network.Socket.ByteString (sendTo, recvFrom)
import System.Environment (getArgs)
import System.Posix.Process (getProcessID)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Ratio (numerator)
-- to run: >$ stack build && sudo stack exec hsping --allow-different-user

timeInMicros :: IO Word64 -- use micros for its precision
timeInMicros = fromIntegral . numerator . toRational . (* 1000000) <$> getPOSIXTime

newtype PID = PID Word16
newtype Sequence = Sequence Word16
newtype PacketsSent = PacketsSent Int
newtype PacketsReceived = PacketsReceived Int

icmpMockData :: BL.ByteString
icmpMockData = let numBytes = 7
           in BL.pack [1..(8*numBytes)]
           
maxReceive = 2048 -- maximum number of bits to receive
ipHeaderLength = 20 -- length of IP address header in bytes
icmpHeaderLength = 8 -- length of ICMP header in bytes
icmpProtocol :: ProtocolNumber
icmpProtocol = 1

-- Build type and data constructors for a strongly typed request
data ICMPRequest = ICMPRequest {
    _type :: Word8
  , _code :: Word8
  , _checksum :: Word16
  , _identifier :: Word16
  , _sequence :: Word16
  , _data :: Word64
}

data Stats = Stats PacketsSent PacketsReceived

getICMPHeader :: Get (Word8, Word8, Word16, Word16, Word16)
getICMPHeader = do
  gType <- getWord8
  gCode <- getWord8
  gChecksum <- getWord16be
  gIdentifier <- getWord16be
  gSequence <- getWord16be
  return (gType, gCode, gChecksum, gIdentifier, gSequence)

getICMPData :: Get Word64
getICMPData = do
  timestamp <- getWord64be
  return timestamp

writeToBuffer :: ICMPRequest -> Put
writeToBuffer icmp = do
  putWord8 $ _type icmp
  putWord8 $ _code icmp
  putWord16be $ _checksum icmp
  putWord16be $ _identifier icmp
  putWord16be $ _sequence icmp
  putWord64be $ _data icmp

buildRequest :: PID -> Sequence -> Word64 -> ICMPRequest
buildRequest (PID pid) (Sequence seq) icmpdata = ICMPRequest 8 0 checksum pid seq icmpdata
  where
    initialBuild :: ICMPRequest
    initialBuild = ICMPRequest 8 0 0 pid seq icmpdata

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

getTtl :: Get Word8
getTtl = do
  ttl <- getWord8
  return ttl

listenForReply :: Int -> Socket -> SockAddr -> PID -> Sequence -> IORef Stats -> IO()
listenForReply bytesSent s sa (PID pid) (Sequence seq) stats = do
  (response, senderAddress) <- recvFrom s maxReceive
  receivedAt <- timeInMicros
  (Stats (PacketsSent sent) (PacketsReceived received)) <- readIORef stats
  let
    (ipHeader, ipData) = B.splitAt ipHeaderLength response  -- separate IP header from the response byte string
    (_, ttlAndRest) = B.splitAt 8 ipHeader
    timetolive = runGet getTtl (BL.fromStrict ttlAndRest)
    (icmpHeader, icmpData) = B.splitAt icmpHeaderLength ipData -- separate ICMP header from the ICMP reply packet
    (gType, gCode, gChecksum, gIdentifier, gSequence) = runGet getICMPHeader (BL.fromStrict icmpHeader)
    timestamp = runGet getICMPData (BL.fromStrict icmpData)
  case ((fromIntegral gIdentifier == pid) && sa == senderAddress) of
    True -> do -- identifiers and host matched, this is the correct ICMP Reply
      let
        ttl = fromIntegral timetolive
        sentAt = fromIntegral timestamp
        timeDeltaInMicros = ((fromIntegral receivedAt) - sentAt)
        timeDeltaInMillis = timeDeltaInMicros / 1000
      writeIORef stats (Stats (PacketsSent (sent + 1)) (PacketsReceived (received + 1)))
      threadDelay $ (10^6) * 1
      _ <- putStrLn $ (show bytesSent) ++ " bytes sent from " ++ (show senderAddress) ++ ": icmp_seq=" ++ (show seq) ++ " ttl=" ++ (show ttl) ++ " time=" ++ (show timeDeltaInMillis) ++ " ms"
      pingHost s sa (PID pid) (Sequence (seq + 1)) stats
    False -> do -- was a different packet, continue listening
      _ <- putStrLn $ "The identifier " ++ (show gIdentifier) ++ " does not match PID " ++ (show pid) ++ ". Continue listening for correct ICMP packet"
      listenForReply bytesSent s sa (PID pid) (Sequence seq) stats


pingHost :: Socket -> SockAddr -> PID -> Sequence -> IORef Stats -> IO()
pingHost s sa (PID pid) (Sequence seq) stats = do
  time <- timeInMicros
  bytesSent <- sendTo s ((B.concat . BL.toChunks . runPut . writeToBuffer) $ buildRequest (PID pid) (Sequence seq) time) sa
  listenForReply bytesSent s sa (PID pid) (Sequence seq) stats 
  
ping :: IO()
ping = do
  args <- getArgs
  if length args == 0 
    then error "Please enter a host address to ping!"
    else do
      pid <- getProcessID
      _ <- putStrLn $ "--- Starting haskell ping service with Process ID " ++ (show pid) ++ " ---"
      sock <- socket AF_INET Raw icmpProtocol
      let host = head args
      addrInfo <- getAddrInfo Nothing (Just host) Nothing -- Don't need to provide hints, since only host matters.
      let sockAddress = addrAddress $ head addrInfo
      stats <- newIORef $ Stats (PacketsSent 0) (PacketsReceived 0) 
      pingHost sock sockAddress (PID (fromIntegral pid)) (Sequence 0) stats `finally` do
        printStats sockAddress stats
        putStrLn "goodbye"

printStats :: SockAddr -> IORef Stats -> IO()
printStats sa s = do
  (Stats (PacketsSent sent) (PacketsReceived received)) <- readIORef s
  let ratio = (fromIntegral received) / (fromIntegral sent) :: Float
  putStrLn "process terminated"
  putStrLn $ "--- " ++ (show sa) ++ " ping statistics ---"
  putStrLn $ (show sent) ++ " packets transmitted, " ++ (show received) ++ " packets received, " ++ (show (100 - (ratio * 100))) ++ "% packet loss"
