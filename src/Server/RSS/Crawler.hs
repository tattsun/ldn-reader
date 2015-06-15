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
import           Server.RSS.Related
----------------------------------------------------------------------

startCrawler :: ContextM ()
startCrawler = do
  ctx <- context
  delayMcs <- (*1000000) . confCrawlerDelaySec . ctxConfig <$> context

  void . liftIO . forkIO . runContextM ctx . forever $ do
    logNorm "Start Crawling All RSS"
    crawlAllRSS
    logNorm "Finished Crawling All RSS"
    liftIO . threadDelay $ delayMcs

crawlAllRSS :: ContextM ()
crawlAllRSS = do
  ctx <- context

  asyncs <- mapM (liftIO . async . runContextM ctx . crawlRSS) newsTags
  mapM_ (liftIO . wait) asyncs

crawlRSS :: NewsTag -> ContextM ()
crawlRSS tag = do
  logNorm $ T.concat ["Crawling ", T.pack . show $ tag]

  rss <- fixRSSNum =<< parseRSS <$> (HTTP.simpleHttp . rssurl $ tag)
  rss' <- addRelatedArticlesRSS rss
  Cache.put tag rss'

  logNorm $ T.concat ["Finished crawling ", T.pack . show $ tag]
  return ()

fixRSSNum :: RSS -> ContextM RSS
fixRSSNum (RSS rss) = do
  num <- confArticleMaxNum . ctxConfig <$> context
  return $ RSS $ take num rss

----------------------------------------------------------------------

rssurl :: NewsTag -> String
rssurl Cin = "http://news.livedoor.com/rss/summary/52.xml"
rssurl tag = "http://news.livedoor.com/topics/rss/" ++ nt2String tag ++ ".xml"

----------------------------------------------------------------------
-- * Debugging
t = do
  startCrawler
  liftIO $ threadDelay 100000000
