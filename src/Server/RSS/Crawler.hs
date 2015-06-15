module Server.RSS.Crawler where

import           Control.Concurrent
--
import           Server.Base
import qualified Server.RSS.Cache   as Cache
import           Server.RSS.Parser
----------------------------------------------------------------------

startCrawler :: ContextM ()
startCrawler = do
  ctx <- context
  delayMcs <- (*1000000) . confCrawlerDelaySec . ctxConfig <$> context

  void . liftIO . forkIO . runContextM ctx . forever $ do
    crawlAllRSS
    liftIO . threadDelay $ delayMcs

crawlAllRSS :: ContextM ()
crawlAllRSS = mapM_ crawlRSS newsTags

crawlRSS :: NewsTag -> ContextM ()
crawlRSS = undefined
