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
    tag <- do
      p <- param "tag"
      if null p
        then return "top"
        else return p
    (RSS articles) <- lift . getRSS . fromJust $ string2Nt tag
    html $ renderHtml $ $(hamletFile "view/index.hamlet") undefined
