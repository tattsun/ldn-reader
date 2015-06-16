{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Server.API where

import           Network.Wai.Middleware.Static
import           Text.Blaze                    (preEscapedText)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Hamlet                   (hamletFile)
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
  get "/:tag" $ do
    tag <- param "tag"
    rss <- lift . getRSS $ defString2Nt tag

    offset <- (read :: String -> Int) <$> paramDefault "offset" "0"
    let articles = drop offset $ unArticleMap rss
--    forM_ articles $ \a -> do
--      lift $ logDebg $ T.concat [ "TITLE: ", asTitle a, "\n"
--                                , "LINK: ", asLink a, "\n"
--                                , "DESC: ", asDescription a, "\n"
--                                ]

    articleNum <- confArticleNumPerPage . ctxConfig <$> lift context

    html $ renderHtml $ $(hamletFile "view/index.hamlet") undefined

defString2Nt :: String -> NewsTag
defString2Nt str = maybe Top id (string2Nt str)
