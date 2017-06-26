module Ping where

import Network.Socket (Family(AF_INET), SocketType(Raw), socket, getSocketName)

helloWorld :: IO()
helloWorld = do
  sock <- socket AF_INET Raw 1
  name <- getSocketName sock
  _ <- putStrLn $ show name
  putStrLn "Hello World!"
