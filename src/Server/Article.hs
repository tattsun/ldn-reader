{-# LANGUAGE OverloadedStrings #-}
module Server.Article where

import           Control.Lens
import           Data.Default
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT
import           Network.HTTP.Conduit
import qualified Text.XML                as XML
import           Text.XML.Lens
--
import           Server.Base
--
import qualified Data.Text.IO            as T
----------------------------------------------------------------------

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
                                , articleGuid = guid
                                , articleRelatedURL = Nothing
                                }
                        ) articleDatum
  in articles
  where
    zip4 [] _ _ _ = []
    zip4 _ [] _ _ = []
    zip4 _ _ [] _ = []
    zip4 _ _ _ [] = []
    zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a,b,c,d) : zip4 as bs cs ds
