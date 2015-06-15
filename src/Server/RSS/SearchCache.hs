{-# LANGUAGE OverloadedStrings #-}
module Server.RSS.SearchCache
       ( add
       , lookup
       ) where

import           Control.Concurrent
import qualified Data.Map           as M
import qualified Data.Text          as T
import           Prelude            hiding (lookup)
--
import           Server.Base        hiding (lookup)
----------------------------------------------------------------------

add :: T.Text -> [RelatedArticle] -> ContextM ()
add key val = do
  (SearchCache mvar) <- ctxSearchCache <$> context
  hm <- liftIO $ takeMVar mvar
  liftIO . putMVar mvar $ M.insert key val hm

lookup :: T.Text -> ContextM (Maybe [RelatedArticle])
lookup key = do
  (SearchCache mvar) <- ctxSearchCache <$> context
  hm <- liftIO $ readMVar mvar
  return $ M.lookup key hm
