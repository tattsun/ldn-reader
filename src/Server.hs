module Server where

import           Control.Concurrent
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
--
import qualified Server.Article     as A
import           Server.Base
import qualified Server.KeyPhrase   as K
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
test = do
  debugRun $ do
    A.startCrawler
  forever $ do
    liftIO $ threadDelay 1000

--  A.test
--  args <- getArgs
--  print =<< (debugRun $ K.getKeyPhrase (args !! 0))
