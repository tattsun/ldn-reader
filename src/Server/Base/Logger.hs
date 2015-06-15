{-# LANGUAGE OverloadedStrings #-}
module Server.Base.Logger where

import           Control.Concurrent
import           Control.Monad
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
----------------------------------------------------------------------

data LogLevel = DEBG | WARN | FATA | NORM
              deriving (Show, Eq, Ord)
type Log = (LogLevel, T.Text)
data Logger = Logger { logChan :: Chan Log
                     }

newLogger :: LogLevel -> IO Logger
newLogger lv = do
  chan <- newChan
  void $ forkIO $ logThread lv chan
  return $ Logger chan

logThread :: LogLevel -> Chan Log -> IO ()
logThread dlv chan = go
  where
    go = do
      (lv, msg) <- readChan chan
      if lv >= dlv
        then do T.putStrLn . T.concat $ [T.pack . show $ lv, ": ", msg]
                go
        else go

writeLog :: Log -> Logger -> IO ()
writeLog log logger = writeChan (logChan logger) log
