{-# LANGUAGE OverloadedStrings #-}
module Server where

import           Web.Scotty.Trans
--
import           Server.API
import           Server.Base
import qualified Server.Base.Logger as Log
import           Server.RSS
----------------------------------------------------------------------

initContext :: IO Context
initContext = do
  conf <- fromJust <$> readConfig "./config.yml"
  news <- initNewsFeeds
  scache <- initSearchCache
  logger <- if confEnvironment conf == Development
            then Log.newLogger Log.DEBG
            else Log.newLogger Log.WARN
  return $ Context { ctxNews = news
                   , ctxSearchCache = scache
                   , ctxLogger = logger
                   , ctxConfig = conf
                   }

run :: IO ()
run = do
  ctx <- initContext
  runContextM ctx $ do
    when (confCrawlerEnabled . ctxConfig $ ctx) startCrawler
  runApp 3000 ctx app

app :: App ()
app = api
