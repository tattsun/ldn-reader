{-# LANGUAGE OverloadedStrings #-}
module Server.RSS.Related
       ( addRelatedArticlesRSS
       ) where

import qualified Data.Text              as T
--
import           Server.Base
import           Server.RSS.KeyPhrase
import           Server.RSS.Search
import qualified Server.RSS.SearchCache as C
----------------------------------------------------------------------

addRelatedArticlesRSS :: RSS -> ContextM RSS
addRelatedArticlesRSS rss =
  RSS <$> mapM addRelatedArticles (unArticleMap rss)

addRelatedArticles :: ArticleSummary -> ContextM ArticleSummary
addRelatedArticles a = do
  logDebg $ T.concat ["Searching related articles of "
                     ,asTitle a]
  cache <- C.lookup . T.pack . asGuid $ a
  case cache of
    Just c -> do logDebg $ T.concat ["Using cache on ", asTitle a]
                 return a { asRelatedArticles = c }
    Nothing -> doSearch
  where
    doSearch = do
      keyphrase <- getKeyPhrase (asTitle a)
      case keyphrase of
        Nothing -> do logWarn $ T.concat ["Keyphrase not found on "
                                         ,asTitle a]
                      return a
        Just k' -> do related <- search k'
                      C.add (T.pack . asGuid $ a) related
                      return a { asRelatedArticles = related }
