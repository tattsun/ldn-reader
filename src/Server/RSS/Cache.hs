{-# LANGUAGE OverloadedStrings #-}
module Server.RSS.Cache
       ( get
       , put
       ) where

import           Control.Concurrent
import qualified Data.Map           as M
--
import           Server.Base
----------------------------------------------------------------------

getMVar :: NewsTag -> ContextM (MVar RSS)
getMVar tag = (M.! tag) . unNewsFeeds . ctxNews <$> context

get :: NewsTag -> ContextM RSS
get tag = getMVar tag >>= liftIO . readMVar

put :: NewsTag -> RSS -> ContextM ()
put tag rss = getMVar tag >>= void . liftIO . flip swapMVar rss
