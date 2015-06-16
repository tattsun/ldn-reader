{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Server.RSS.Search
       ( search
       ) where

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Default
import qualified Data.Text                  as T
import qualified Network.HTTP.Conduit       as HTTP
import qualified Text.XML                   as XML
import           Text.XML.Lens
--
import           Server.Base
----------------------------------------------------------------------

parseResult :: LBS.ByteString -> [RelatedArticle]
parseResult xml =
  let doc = XML.parseLBS_ def xml
      getTxts t = doc ^.. root ./ el "channel" ./ el "item" ./ el t . text

      titles = take 3 $ getTxts "title"
      links = take 3 $ getTxts "link"

      datum = zip titles links
  in map (\(title, link) -> RelatedArticle title link) datum

----------------------------------------------------------------------

search :: T.Text -> ContextM [RelatedArticle]
search keyword = do
  url <- searchUrl keyword
  json <- liftIO . HTTP.simpleHttp $ url
  return $ parseResult json

----------------------------------------------------------------------

searchUrl :: T.Text -> ContextM String
searchUrl t = do
  useSample <- confUseSampleXml . ctxConfig <$> context
  if useSample
    then return $ "http://127.0.0.1:3000/xml/sample.xml?q=" ++ T.unpack t
    else return $ "http://www.bing.com/news/search?format=RSS&q=" ++ T.unpack t
