module Server where

import qualified Data.Text        as T
import qualified Data.Text.IO     as T
--
import           Server.Article
import           Server.Base
import qualified Server.KeyPhrase as K
----------------------------------------------------------------------

{-
test = debugRun $ do
  txt <- liftIO $ httpGet $ rssUrl Cin
  articles <- getArticles txt
  mapM_ (liftIO . T.putStrLn . showArticle) articles
-}

test :: IO ()
test = debugRun $ K.test "私の名前は、やふーです。"
