{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Server.RSS.Search
       ( search
       ) where

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text                  as T
import qualified Network.HTTP.Conduit       as HTTP
--
import           Server.Base
----------------------------------------------------------------------

data APIResponseResult = APIResponseResult { arr_unescapedUrl      :: T.Text
                                           , arr_titleNoFormatting :: T.Text
                                           }
                         deriving (Show)
data APIResponseData = APIResponseData { ard_results :: [APIResponseResult]
                                       }
                       deriving (Show)
data APIResponse = APIResponse { ar_responseData :: APIResponseData
                               }
                   deriving (Show)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 4} ''APIResponseResult)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 4} ''APIResponseData)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 3} ''APIResponse)

parseResult :: LBS.ByteString -> [RelatedArticle]
parseResult str =
  let decoded = decode str
  in case decoded of
  (Just (APIResponse (APIResponseData xs))) ->
    map (\x -> RelatedArticle (arr_titleNoFormatting x) (arr_unescapedUrl x)) xs
  Nothing -> []


----------------------------------------------------------------------

search :: T.Text -> ContextM [RelatedArticle]
search keyword = do
  json <- liftIO . HTTP.simpleHttp $ searchUrl keyword
  logDebg $ T.pack $ searchUrl keyword
  logDebg $ T.pack . show $ json
  return $ parseResult json

----------------------------------------------------------------------

searchUrl :: T.Text -> String
searchUrl = (++) "http://ajax.googleapis.com/ajax/services/search/news?v=1.0&q=" . T.unpack
