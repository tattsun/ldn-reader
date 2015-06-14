{-# LANGUAGE OverloadedStrings #-}
module Server.KeyPhrase
       ( getKeyPhrase
       ) where

import           Data.Default
import qualified Data.Text            as T
import qualified Network.HTTP.Base    as HTTP
import           Network.HTTP.Conduit
import qualified Text.XML             as XML
import           Text.XML.Lens
--
import           Server.Base
----------------------------------------------------------------------

baseUrl :: String
baseUrl = "http://jlp.yahooapis.jp/KeyphraseService/V1/extract"

mkUrl :: String -> ContextM String
mkUrl sentence = do
  appId <- confYahooApplicationID . ctxConfig <$> context
  let query = concat ["?appid=", appId
                     ,"&sentence=", HTTP.urlEncode sentence
                     ,"&output=xml"]
  return $ baseUrl ++ query

getKeyPhrases :: String -> ContextM [T.Text]
getKeyPhrases sentence = do
  url <- mkUrl sentence
  res <- simpleHttp url
  let doc = XML.parseLBS_ def res
      phrases = doc ^.. root . ell "ResultSet" ./ ell "Result" ./ ell "Keyphrase" . text
  return phrases

getKeyPhrase :: String -> ContextM (Maybe T.Text)
getKeyPhrase sentence = do
  phrases <- getKeyPhrases sentence
  if null phrases
    then return Nothing
    else return . Just . head $ phrases
