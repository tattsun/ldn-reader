{-# LANGUAGE OverloadedStrings #-}
module Server.KeyPhrase where

import qualified Network.HTTP.Base          as HTTP
import           Network.HTTP.Conduit
--
import           Server.Base
--
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Encoding    as LT
import qualified Data.Text.Lazy.IO          as LT
----------------------------------------------------------------------

baseUrl :: String
baseUrl = "http://jlp.yahooapis.jp/KeyphraseService/V1/extract"

mkUrl :: String -> ContextM String
mkUrl sentence = do
  appId <- confYahooApplicationID . ctxConfig <$> context
  let query = concat ["?appid=", appId
                     ,"&sentence=", HTTP.urlEncode sentence
                     ,"&output=json"]
  return $ baseUrl ++ query

getKeyPhrase sentence = do
  url <- mkUrl sentence
  simpleHttp url

test k = do
  ph <- getKeyPhrase k
  liftIO $ LT.putStrLn . LT.decodeUtf8 $ ph
