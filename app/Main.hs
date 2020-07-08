
module Main where

import Control.Monad.STM
import Control.Concurrent.STM.TChan 
import Control.Concurrent
import Control.Monad
import NodeRepl 

main :: IO ()
main = do 
  (write, read) <- nodeRepl 
  forkIO $ readChan read 
  go write
  where  
    readChan chan = forever $ do 
      s <- atomically $ readTChan chan 
      putStrLn s 

    go w = do
      line <- getLine 
      if line == ".quit"
        then return ()
        else do 
          atomically $ writeTChan w line 
          go w