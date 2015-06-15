{-# LANGUAGE OverloadedStrings #-}
module Server.RSS.Crawler where

import           Control.Concurrent
import           Control.Concurrent.Async
import qualified Data.Text                as T
import qualified Network.HTTP.Conduit     as HTTP
--
import           Server.Base
import qualified Server.RSS.Cache         as Cache
import           Server.RSS.Parser
----------------------------------------------------------------------

startCrawler :: ContextM ()
startCrawler = do
  ctx <- context
  delayMcs <- (*1000000) . confCrawlerDelaySec . ctxConfig <$> context

  void . liftIO . forkIO . runContextM ctx . forever $ do
    logDebg "Start Crawling All RSS"
    crawlAllRSS
    logDebg "Finished Crawling All RSS"
    liftIO . threadDelay $ delayMcs

crawlAllRSS :: ContextM ()
crawlAllRSS = do
  ctx <- context

  asyncs <- mapM (liftIO . async . runContextM ctx . crawlRSS) newsTags
  mapM_ (liftIO . wait) asyncs

crawlRSS :: NewsTag -> ContextM ()
crawlRSS tag = do
  logDebg $ T.concat ["Crawling ", T.pack . show $ tag]

  rss <- parseRSS <$> (HTTP.simpleHttp . rssurl $ tag)
  Cache.put tag rss

  logDebg $ T.concat ["Finished crawling ", T.pack . show $ tag]
  return ()

----------------------------------------------------------------------

rssurl :: NewsTag -> String
rssurl Cin = "http://news.livedoor.com/rss/summary/52.xml"
rssurl tag = "http://news.livedoor.com/topics/rss/" ++ nt2String tag ++ ".xml"

----------------------------------------------------------------------
-- * Debugging
t = do
  startCrawler
  liftIO $ threadDelay 100000000
