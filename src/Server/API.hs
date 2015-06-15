{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Server.API where

import           Text.Blaze                    (preEscapedText)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Hamlet                   (hamletFile)
import           Web.Scotty.Trans
--
import           Server.Base
import           Server.RSS
----------------------------------------------------------------------

api :: App ()
api = do
  get "/:tag" $ do
    tag <- param "tag"
    (RSS articles) <- lift . getRSS . fromJust $ string2Nt tag
    html $ renderHtml $ $(hamletFile "view/index.hamlet") undefined
