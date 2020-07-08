{-# LANGUAGE OverloadedStrings #-}
module NodeRepl
    ( nodeRepl
    ) where

import Control.Monad.STM
import Control.Concurrent.STM.TChan 
import Control.Concurrent
import Control.Monad
import System.IO
import System.Process


nodeRepl :: IO (TChan String, TChan String) 
nodeRepl = do
  tchanRead  <- newTChanIO
  tchanWrite <- newTChanIO

  (hin,hout,herr,pl) <- runInteractiveCommand "node -e \"require('repl').start({ignoreUndefined: true, prompt: ''})\""
  hSetBinaryMode hin False
  hSetBinaryMode hout False
  hSetBuffering hin LineBuffering
  hSetBuffering hout NoBuffering


  hPutStrLn hin ".load ./assets/session.js"
  threadId <- forkIO $ forever $ do 
                input <- atomically $ readTChan tchanRead
                hPutStrLn hin input 

  threadId <- forkIO $ forever $ do 
                isEOF <- hIsEOF hout
                unless isEOF $ do 
                  out <- hGetLine hout
                  if null out
                    then return () 
                    else atomically $ writeTChan tchanWrite out 
              
  return (tchanRead, tchanWrite)