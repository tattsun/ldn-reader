{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Server.Base
       ( -- * Exposed Modules
         module X
         -- * TypeClasses
       , Config(..)
       , Context(..)
       , ContextM
       , App
       , Act
         -- * Monad Runner
       , runContextM
       , runApp
         -- * Environment Reader
       , context
         -- * Logging
       , logNorm
       , logFata
       , logWarn
       , logDebg
         -- * Article
       , ArticleTag(..)
       , articleTags
       , at2String
       , string2At
       , News(..)
       , ArticleMap(..)
       , Article(..)
       , initNews
         -- * Debugging
       , debugRun
       ) where

import           Control.Applicative      as X
import           Control.Monad            as X
import           Control.Monad.IO.Class   as X
import           Data.Either              as X
import           Data.Maybe               as X
--
import           Control.Concurrent.MVar
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Aeson.TH
import qualified Data.Map                 as M
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as LT
import qualified Data.Yaml                as Yaml
import           Network.Wai.Handler.Warp (Port)
import           Web.Scotty.Trans         hiding (get, put)
--
import qualified Server.Base.Logger       as Log
----------------------------------------------------------------------

data ArticleTag = Top | Dom | Int
                | Eco | Ent | Spo
                | Cin | Gourmet | Love | Trend
                deriving (Show, Eq, Ord)
articleTags = [Top, Dom, Int
              ,Eco, Ent, Spo
              ,Cin, Gourmet, Love, Trend]
at2String :: ArticleTag -> String
at2String Top = "top"
at2String Dom = "dom"
at2String Int = "int"
at2String Eco = "eco"
at2String Ent = "ent"
at2String Spo = "spo"
at2String Cin = "52"
at2String Gourmet = "gourmet"
at2String Love = "love"
at2String Trend = "trend"
string2At :: String -> Maybe ArticleTag
string2At "top" = Just Top
string2At "dom" = Just Dom
string2At "int" = Just Int
string2At "eco" = Just Eco
string2At "ent" = Just Ent
string2At "spo" = Just Spo
string2At "52" = Just Cin
string2At "gourmet" = Just Gourmet
string2At "love" = Just Love
string2At "trend" = Just Trend
string2At _ = Nothing

data News = News { unNews :: M.Map ArticleTag (MVar ArticleMap) }
data ArticleMap = ArticleMap { amLatestList :: [T.Text]
                             , amArticles   :: M.Map T.Text Article }
data Article = Article { articleTitle       :: T.Text
                       , articleLink        :: T.Text
                       , articleDescription :: T.Text
                       , articleBody        :: Maybe T.Text
                       , articleGuid        :: T.Text
--                       , articlePubDate :: T.Text
                       , articleRelatedURL  :: Maybe [T.Text] }
               deriving (Show)

initNews :: IO News
initNews = News <$> hm
  where
    hm :: IO (M.Map ArticleTag (MVar ArticleMap))
    hm = flip execStateT M.empty $ forM_ articleTags $ \tag -> do
      m <- get
      mvar <- liftIO $ newMVar $ ArticleMap [] M.empty
      put $ M.insert tag mvar m

----------------------------------------------------------------------

data Config = Config { confYahooApplicationID :: String
                     , confYahooSecretKey     :: String
                     }
              deriving (Show)
$(deriveJSON defaultOptions{ fieldLabelModifier = drop 4 } ''Config)

readConfig :: FilePath -> IO (Maybe Config)
readConfig fp = Yaml.decodeFile fp

data Context = Context { ctxNews   :: News
                       , ctxLogger :: Log.Logger
                       , ctxConfig :: Config
                       }
newtype ContextM a = ContextM { unContextM :: ReaderT Context IO a }
                     deriving (Monad, Applicative, Functor, MonadReader Context, MonadIO)

runContextM :: Context -> ContextM a -> IO a
runContextM ctx = flip runReaderT ctx . unContextM

context :: ContextM Context
context = ask

type App = ScottyT LT.Text ContextM
type Act = ActionT LT.Text ContextM

runApp :: Port -> Context -> App () -> IO ()
runApp port ctx = scottyT port (runContextM ctx) (runContextM ctx)

----------------------------------------------------------------------

log_ :: Log.LogLevel -> T.Text -> ContextM ()
log_ lv msg = do
  logger <- ctxLogger <$> context
  liftIO $ Log.writeLog (lv, msg) logger

logNorm = log_ Log.NORM
logFata = log_ Log.FATA
logWarn = log_ Log.WARN
logDebg = log_ Log.DEBG

----------------------------------------------------------------------


debugRun :: ContextM a -> IO a
debugRun m = do
  news <- initNews
  logger <- Log.newLogger Log.DEBG
  conf <- fromJust <$> readConfig "./config.yml"
  let ctx = Context { ctxNews = news
                    , ctxLogger = logger
                    , ctxConfig = conf
                    }
  runContextM ctx m
