module Ping where

import Data.Bits
import Data.Word (Word8, Word16, Word32)
import Data.Binary.Get (Get, getWord16be, isEmpty, runGet)
import Data.Binary.Put (Put, putWord8, putWord16be, putLazyByteString, runPut)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Network.Socket (Family(AF_INET), Socket, SocketType(Raw), SockAddr(SockAddrInet),addrAddress,addrFamily, addrProtocol, addrSocketType, ProtocolNumber, connect,isConnected, getAddrInfo, socket, close)
import Network.Socket.ByteString (sendTo, recv)
import System.Posix.Process (getProcessID)

-- to run: >$ stack build && sudo stack exec hsping --allow-different-user

-- Util function for testing binary
toBinary :: Bits a => a -> [Char]
toBinary x = fmap (\y -> if (testBit x (fromIntegral y)) then '1' else '0') [7,6..0]

type PID = Word16

icmpData :: BL.ByteString
icmpData = let numBytes = 7
           in BL.pack [1..(8*numBytes)]

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

buildRequest :: PID -> BL.ByteString -> ICMPRequest
buildRequest pid icmpdata = ICMPRequest 8 0 checksum pid 1 icmpdata
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

    total :: Word16
    total = (sum values) + 4

    checksum :: Word16
    checksum = complement $ fromIntegral total

writeToBuffer :: ICMPRequest -> Put
writeToBuffer icmp = do
  putWord8 $ _type icmp
  putWord8 $ _code icmp
  putWord16be $ _checksum icmp
  putWord16be $ _identifier icmp
  putWord16be $ _sequence icmp
  putLazyByteString $ _data icmp

helloWorld :: IO()
helloWorld = do
  pid <- getProcessID
  _ <- putStrLn $ "--- Starting haskell ping service with Process ID " ++ (show pid) ++ " ---"
  sock <- socket AF_INET Raw icmpProtocol
  addrInfo <- getAddrInfo Nothing (Just "127.0.0.1") Nothing -- Don't need to provide hints, since only host matters.
  let sockAddress = addrAddress $ head addrInfo
  _ <- putStrLn $ "Data: " ++ (show $ icmpData)
  _ <- putStrLn $ "Socket Address: " ++ (show $ sockAddress)
  bytesSent <- sendTo sock ((B.concat . BL.toChunks . runPut . writeToBuffer) $ buildRequest (fromIntegral pid) icmpData) sockAddress
  _ <- putStrLn $ "Number of Bytes Sent: " ++ (show bytesSent)
  putStrLn "goodbye"
