module Ping where

import Network.Socket

helloWorld :: IO()
helloWorld = do
  putStrLn "Hello World!"
