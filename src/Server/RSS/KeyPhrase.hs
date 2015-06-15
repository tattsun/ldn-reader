{-# LANGUAGE OverloadedStrings #-}
module Server.RSS.Keyphrase
       ( getKeyPhrase
       ) where

import           Data.Default
import qualified Data.Text            as T
import           Network.HTTP.Base    (urlEncode)
import qualified Network.HTTP.Conduit as HTTP
import qualified Text.XML             as XML
import           Text.XML.Lens
--
import           Server.Base
----------------------------------------------------------------------

getKeyPhrases :: T.Text -> ContextM [T.Text]
getKeyPhrases src = do
  url <- requestUrl src
  doc <- XML.parseLBS_ def <$> liftIO (HTTP.simpleHttp url)

  let phrases = doc ^.. root . ell "ResultSet" ./ ell "Result" ./ ell "Keyphrase" . text

  return phrases

getKeyPhrase :: T.Text -> ContextM (Maybe T.Text)
getKeyPhrase src = do
  kps <- getKeyPhrases src
  if null kps
    then return Nothing
    else do
    logDebg $ head kps
    return . Just . head $ kps

----------------------------------------------------------------------

baseurl :: String
baseurl = "http://jlp.yahooapis.jp/KeyphraseService/V1/extract"

requestUrl :: T.Text -> ContextM String
requestUrl src = do
  appid <- confYahooApplicationID . ctxConfig <$> context
  return $
    concat [ baseurl
           , "?appid=", appid
           , "&sentence=", urlEncode $ T.unpack src
           , "&output=xml"
           ]
