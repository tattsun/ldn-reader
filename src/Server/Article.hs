{-# LANGUAGE OverloadedStrings #-}
module Server.Article where

import           Codec.Text.IConv
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Lens
import           Control.Monad.State
import           Data.Default
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Encoding    as LT
import           Network.HTTP.Conduit
import           Text.HTML.DOM              as HTML
import qualified Text.XML                   as XML
import           Text.XML.Lens
--
import           Server.Base
--
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.IO               as T
import qualified Data.Text.Lazy.IO          as LT
----------------------------------------------------------------------

startCrawler :: ContextM ()
startCrawler = do
  ctx <- context
  delay <- confCrawlerDelaySec . ctxConfig <$> context
  void $ liftIO $ forkIO $ runContextM ctx $ forever $ do
    crawlRss
    liftIO . threadDelay $ delay * 1000000

crawlRss :: ContextM ()
crawlRss = do
  ctx <- context
  newshm <- unNews . ctxNews <$> context
  let acts = flip map (M.toList newshm) $ \(tag, mvar) -> do
        runContextM ctx (crawlTag tag mvar)
          `catch` (\(SomeException e) -> runContextM ctx (logFata . T.pack . show $ e))
  asyncs <- mapM (liftIO . async) acts
  liftIO $ mapM wait asyncs
  logNorm "Crawling Complete"
  return ()

crawlTag :: ArticleTag -> MVar ArticleMap -> ContextM ()
crawlTag tag amapMvar = do
  logNorm $ T.concat ["Start crawling on ", T.pack (show tag)]
  logNorm $ T.concat ["Downloading ", T.pack $ rssUrl tag, "..."]
  rss <- liftIO . httpGet $ rssUrl tag
  let articles = parseArticleList rss
  amap <- liftIO $ readMVar amapMvar
  hmNew <- flip execStateT (amArticles amap) $ forM_ articles $ \a -> do
    hm <- get
    let guid = articleGuid a
    if T.pack guid `elem` M.keys hm
      then return ()
      else do body <- liftIO $ getArticleBody guid
              put $ M.insert (T.pack guid) (a { articleBody = Just body }) hm
              lift . logNorm . T.concat $ ["New Article: ", T.pack guid]
              return ()
  let amapnew = amap { amLatestList = map (T.pack . articleGuid) articles
                     , amArticles = hmNew
                     }
  liftIO $ swapMVar amapMvar amapnew
  logNorm $ T.concat ["Finished ", T.pack (show tag)]
  return ()

rssUrl :: ArticleTag -> String
rssUrl Cin = "http://news.livedoor.com/rss/summary/52.xml"
rssUrl t =
  let url tag = "http://news.livedoor.com/topics/rss/" ++ at2String tag ++ ".xml"
  in url t

httpGet :: String -> IO LT.Text
httpGet url = LT.decodeUtf8 <$> simpleHttp url

parseArticleList :: LT.Text -> [Article]
parseArticleList txt =
  let doc = XML.parseText_ def txt
      getTexts t = doc ^.. root ./ el "channel" ./ el "item" ./ el t . text
      titles = getTexts "title"
      links = getTexts "link"
      descriptions = getTexts "description"
      guids = getTexts "guid"
      articleDatum = zip4 titles links descriptions guids
      articles = map (\(title, link, desc, guid) ->
                        Article { articleTitle = title
                                , articleLink = link
                                , articleDescription = desc
                                , articleBody = Nothing
                                , articleGuid = T.unpack guid
                                , articleRelatedURL = Nothing
                                }
                        ) articleDatum
  in articles

getArticleBody :: String -> IO T.Text
getArticleBody url = do
  txt <- LBS.fromStrict . LBS.toStrict <$> simpleHttp url
  let decoded = convert "EUC-JP" "UTF-8" txt
      doc = HTML.parseLBS decoded
  let apat = T.concat $ doc ^.. root ./
             el "body" ./
             el "div" . attributeIs "id" "container" ./
             el "div" . attributeIs "id" "content" ./
             el "div" . attributeIs "class" "contentInner" ./
             el "div" . attributeIs "id" "main" ./
             el "div" . attributeIs "class" "mainInner" ./
             el "div" . attributeIs "id" "article-body" ./
             el "article" ./
             el "div" . attributeIs "class" "articleBody" ./
             el "span" . attributeIs "itemprop" "articleBody" ./
             el "p" .
             text
      bpat = T.concat $ doc ^.. root ./
             el "body" ./
             el "div" . attributeIs "id" "container" ./
             el "div" . attributeIs "id" "content" ./
             el "div" . attributeIs "class" "contentInner" ./
             el "div" . attributeIs "id" "main" ./
             el "div" . attributeIs "class" "mainInner" ./
             el "div" . attributeIs "id" "article-body" ./
             el "article" ./
             el "div" . attributeIs "class" "articleBody" ./
             el "span" . attributeIs "itemprop" "articleBody" .
             text
  if T.length apat > T.length bpat
    then return apat
    else return bpat
