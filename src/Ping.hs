module Ping where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Data.Bits
import Data.Binary.Get (Get, getWord8, getWord16be, getWord32be, getWord64be, isEmpty, runGet)
import Data.Binary.Put (Put, putWord8, putWord16be, putWord64be, putLazyByteString, runPut)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Ratio (numerator)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Network.Socket (Family(AF_INET), Socket, SocketType(Raw), SockAddr(SockAddrInet),addrAddress,addrFamily, addrProtocol, addrSocketType, ProtocolNumber, connect,isConnected, getAddrInfo, socket, close)
import Network.Socket.ByteString (sendTo, recvFrom)
import System.Environment (getArgs)
import System.Posix.Process (getProcessID)
import System.Timeout (timeout)

-- to run: >$ stack build && sudo stack exec hsping --allow-different-user

second = 10^6
maxReceive = 2048 -- maximum number of bits to receive
ipHeaderLength = 20 -- length of IP address header in bytes
icmpHeaderLength = 8 -- length of ICMP header in bytes
icmpProtocol :: ProtocolNumber
icmpProtocol = 1

timeInMicros :: IO Word64 -- use micros for its precision
timeInMicros = fromIntegral . numerator . toRational . (* 1000000) <$> getPOSIXTime

printStats :: SockAddr -> IORef Stats -> IO()
printStats sa s = do
  (Stats (PacketsSent sent) (PacketsReceived received)) <- readIORef s
  let ratio = (fromIntegral received) / (fromIntegral sent) :: Float
  putStrLn " process terminated"
  putStrLn $ "--- " ++ (show sa) ++ " ping statistics ---"
  putStrLn $ (show sent) ++ " packets transmitted, " ++ (show received) ++ " packets received, " ++ (show (100 - (ratio * 100))) ++ "% packet loss"


newtype TTL = TTL Word8
newtype Type = Type Word8
newtype Code = Code Word8
newtype Checksum = Checksum Word16
newtype PID = PID Word16 deriving (Eq, Show)
newtype Sequence = Sequence Word16
newtype ICMPData = ICMPData Word64
newtype PacketsSent = PacketsSent Int
newtype PacketsReceived = PacketsReceived Int

data ICMPHeader = ICMPHeader Type Code Checksum PID Sequence
data Stats = Stats PacketsSent PacketsReceived

data ICMPRequest = ICMPRequest {
    _type :: Type
  , _code :: Code
  , _checksum :: Checksum
  , _identifier :: PID
  , _sequence :: Sequence
  , _timestamp :: ICMPData  -- Will contain a timestamp here to calculate metrics
}

getICMPHeader :: Get ICMPHeader
getICMPHeader = do
  gType <- getWord8
  gCode <- getWord8
  gChecksum <- getWord16be
  gIdentifier <- getWord16be
  gSequence <- getWord16be
  return $ ICMPHeader (Type gType) (Code gCode) (Checksum gChecksum) (PID gIdentifier) (Sequence gSequence)

getTtl :: Get TTL
getTtl = TTL <$> getWord8

getICMPData :: Get ICMPData
getICMPData = ICMPData <$> getWord64be

writeToBuffer :: ICMPRequest -> Put
writeToBuffer icmp = do
  let
    (Type t) = _type icmp
    (Code co) = _code icmp
    (Checksum ch) = _checksum icmp
    (PID p) = _identifier icmp
    (Sequence s) = _sequence icmp
    (ICMPData d) = _timestamp icmp
  putWord8 $ t
  putWord8 $ co
  putWord16be $ ch
  putWord16be $ p
  putWord16be $ s
  putWord64be $ d

buildRequest :: PID -> Sequence -> ICMPData -> ICMPRequest
buildRequest pid seq icmpdata = ICMPRequest (Type 8) (Code 0) (Checksum checksum) pid seq icmpdata
  where
    initialBuild :: ICMPRequest
    initialBuild = ICMPRequest (Type 8) (Code 0) (Checksum 0) pid seq icmpdata

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

listenForReply :: Int -> Socket -> SockAddr -> PID -> Sequence -> IORef Stats -> IO()
listenForReply bytesSent s sa pid (Sequence seq) stats = do
  -- (response, senderAddress) <- recvFrom s maxReceive
  response <- timeout second (recvFrom s maxReceive)
  (Stats (PacketsSent sent) (PacketsReceived received)) <- readIORef stats
  case response of
    Just ((reply, senderAddress)) -> do
      receivedAt <- timeInMicros
      let
        (ipHeader, ipData) = B.splitAt ipHeaderLength reply  -- separate IP header from the response byte string
        (_, ttlAndRest) = B.splitAt 8 ipHeader
        (TTL timetolive) = runGet getTtl (BL.fromStrict ttlAndRest)
        (icmpHeader, icmpData) = B.splitAt icmpHeaderLength ipData -- separate ICMP header from the ICMP reply packet
        (ICMPHeader _ _ _ replyPID _) = runGet getICMPHeader (BL.fromStrict icmpHeader)
        (ICMPData timestamp) = runGet getICMPData (BL.fromStrict icmpData)
      if ((replyPID == pid) && sa == senderAddress)
      then do -- identifiers and host matched, this is the correct ICMP Reply
        let
          ttl = fromIntegral timetolive
          sentAt = fromIntegral timestamp
          timeDeltaInMicros = ((fromIntegral receivedAt) - sentAt)
          timeDeltaInMillis = timeDeltaInMicros / 1000
        _ <- writeIORef stats (Stats (PacketsSent (sent + 1)) (PacketsReceived (received + 1)))
        _ <- threadDelay $ second
        _ <- putStr $ (show bytesSent) ++ " bytes sent from " ++ (show senderAddress)
        _ <- putStr $ ": icmp_seq=" ++ (show seq) ++ " ttl=" ++ (show ttl)
        _ <- putStrLn $ " time=" ++ (show timeDeltaInMillis) ++ " ms"
        pingHost s sa pid (Sequence (seq + 1)) stats
      else do -- was a different packet, continue listening
        _ <- putStr $ "The identifier " ++ (show replyPID) ++ " does not match PID "
        _ <- putStrLn $ (show pid) ++ ". Continue listening for correct ICMP packet"
        listenForReply bytesSent s sa pid (Sequence seq) stats
    Nothing -> do 
      putStrLn "timed out waiting for a reply"
      writeIORef stats (Stats (PacketsSent (sent + 1)) (PacketsReceived (received)))
      pingHost s sa pid (Sequence (seq + 1)) stats


pingHost :: Socket -> SockAddr -> PID -> Sequence -> IORef Stats -> IO()
pingHost s sa (PID pid) (Sequence seq) stats = do
  time <- timeInMicros
  bytesSent <- sendTo s ((B.concat . BL.toChunks . runPut . writeToBuffer) $ buildRequest (PID pid) (Sequence seq) (ICMPData time)) sa
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

