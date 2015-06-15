{-# LANGUAGE OverloadedStrings #-}
module Server.RSS.Parser
       ( parseRSS
       ) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Default
import qualified Data.Text                  as T
import qualified Text.XML                   as XML
import           Text.XML.Lens
--
import           Server.Base
----------------------------------------------------------------------

parseRSS :: LBS.ByteString -> RSS
parseRSS xml =
  let doc = XML.parseLBS_ def xml
      getTxts t = doc ^.. root ./ el "channel" ./ el "item" ./ el t . text

      titles = getTxts "title"
      links = getTxts "link"
      descriptions = getTxts "description"
      guids = getTxts "guid"

      datum = zip4 titles links descriptions guids

  in RSS $ map (\(title, link, desc, guid) ->
                  ArticleSummary { asTitle = title
                                 , asLink = link
                                 , asDescription = desc
                                 , asGuid = T.unpack guid
                                 , asRelatedArticles = []
                                 }
               ) datum
