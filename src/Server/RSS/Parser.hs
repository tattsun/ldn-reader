{-# LANGUAGE OverloadedStrings #-}
module Server.RSS.Parser
       ( parseRSS
       ) where

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Default
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.UnixTime
import qualified Text.XML                   as XML
import           Text.XML.Lens
--
import           Server.Base
----------------------------------------------------------------------

parseRSS :: LBS.ByteString -> ContextM RSS
parseRSS xml =
  let doc = XML.parseLBS_ def xml
      getTxts t = doc ^.. root ./ el "channel" ./ el "item" ./ el t . text

      titles = getTxts "title"
      links = getTxts "link"
      descriptions = getTxts "description"
      pubdates = getTxts "pubDate"
      guids = getTxts "guid"

      datum = zip5 titles links descriptions pubdates guids

  in sortByPubDate . RSS <$> mapM (\(title, link, desc, pubdate, guid) ->
                     let date = parseUnixTime mailDateFormat .
                                BS.pack . T.unpack $ pubdate
                     in do
                       formattedDate <- liftIO $ formatUnixTime fmt date
                       return $
                         ArticleSummary { asTitle = title
                                        , asLink = link
                                        , asDescription = desc
                                        , asGuid = T.unpack guid
                                        , asPubDate = date
                                        , asPubDateFormatted = T.decodeUtf8 formattedDate
                                        , asRelatedArticles = []
                                        }
                  ) datum

sortByPubDate :: RSS -> RSS
sortByPubDate (RSS xs) = RSS . flip sortBy xs $ \l r ->
  asPubDate r `compare` asPubDate l

fmt :: Format
fmt = "%Y-%m-%d %H:%M:%S"
