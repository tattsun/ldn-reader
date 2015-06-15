module Server where

import           Control.Concurrent
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
--
import           Server.Base
import           Server.RSS
--
import           System.Environment
----------------------------------------------------------------------

{-
test = debugRun $ do
  txt <- liftIO $ httpGet $ rssUrl Cin
  articles <- getArticles txt
  mapM_ (liftIO . T.putStrLn . showArticle) articles
-}

test :: IO ()
test = debugRun $ do
  startCrawler
  liftIO $ forever $ threadDelay 100000

--  A.test
--  args <- getArgs
--  print =<< (debugRun $ K.getKeyPhrase (args !! 0))
