{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Server.API where

import           Data.Default
import           Network.Wai.Middleware.Gzip
import           Network.Wai.Middleware.Static
import           Text.Blaze                    (preEscapedText)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Hamlet                   (hamletFile)
import           Text.Julius                   (juliusFile, renderJavascript)
import           Web.Scotty.Trans
--
import           Server.Base
import           Server.RSS
--
import qualified Data.Text                     as T
----------------------------------------------------------------------

api :: App ()
api = do
  middleware $ staticPolicy $ addBase "asset"
  middleware $ gzip $ def { gzipFiles = GzipCompress }
  get "/m/:tag" $ mainView True
  get "/:tag" $ mainView False

mainView :: Bool -> Act ()
mainView isMobile = do
    tag <- param "tag"
    rss <- lift . getRSS $ defString2Nt tag

    offset <- (read :: String -> Int) <$> paramDefault "offset" "0"
    let articles = drop offset $ unArticleMap rss

    articleNum <- confArticleNumPerPage . ctxConfig <$> lift context

    html $ renderHtml $ $(hamletFile "view/index.hamlet") undefined


defString2Nt :: String -> NewsTag
defString2Nt str = maybe Top id (string2Nt str)
