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
       , Environment(..)
         -- * Monad Runner
       , runContextM
       , runApp
         -- * Environment Reader
       , context
         -- * Scotty Support
       , paramMaybe
       , paramDefault
         -- * Logging
       , logNorm
       , logFata
       , logWarn
       , logDebg
         -- * Config
       , readConfig
         -- * RSS
       , NewsTag(..)
       , newsTags
       , nt2String
       , string2Nt
       , NewsFeeds(..)
       , RSS(..)
       , ArticleSummary(..)
       , initNewsFeeds
       , RelatedArticle(..)
       , SearchCache(..)
       , initSearchCache
         -- * Debugging
       , debugRun
       ) where

import           Control.Applicative      as X
import           Control.Monad            as X
import           Control.Monad.IO.Class   as X
import           Control.Monad.Trans      as X
import           Data.Either              as X
import           Data.List                as X
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

data Environment = Production | Development
                 deriving (Show, Eq, Ord)
$(deriveJSON defaultOptions ''Environment)

----------------------------------------------------------------------

data NewsTag = Top | Dom | Int
             | Eco | Ent | Spo
             | Cin | Gourmet | Love | Trend
             deriving (Show, Eq, Ord)
newsTags = [Top, Dom, Int
           ,Eco, Ent, Spo
           ,Cin, Gourmet, Love, Trend]
nt2String :: NewsTag -> String
nt2String Top = "top"
nt2String Dom = "dom"
nt2String Int = "int"
nt2String Eco = "eco"
nt2String Ent = "ent"
nt2String Spo = "spo"
nt2String Cin = "52"
nt2String Gourmet = "gourmet"
nt2String Love = "love"
nt2String Trend = "trend"
string2Nt :: String -> Maybe NewsTag
string2Nt "top" = Just Top
string2Nt "dom" = Just Dom
string2Nt "int" = Just Int
string2Nt "eco" = Just Eco
string2Nt "ent" = Just Ent
string2Nt "spo" = Just Spo
string2Nt "52" = Just Cin
string2Nt "gourmet" = Just Gourmet
string2Nt "love" = Just Love
string2Nt "trend" = Just Trend
string2Nt _ = Nothing

data NewsFeeds = NewsFeeds { unNewsFeeds :: M.Map NewsTag (MVar RSS) }
data RSS = RSS { unArticleMap :: [ArticleSummary] }
data ArticleSummary = ArticleSummary { asTitle           :: T.Text
                                     , asLink            :: T.Text
                                     , asDescription     :: T.Text
                                     , asGuid            :: String
--                       , articlePubDate :: T.Text
                                     , asRelatedArticles :: [RelatedArticle] }
                deriving (Show)
data RelatedArticle = RelatedArticle { raTitle :: T.Text
                                     , raUrl   :: T.Text
                                     } deriving (Show)
data SearchCache = SearchCache { unSearchCache ::
                                     MVar (M.Map T.Text [RelatedArticle])
                               }

initNewsFeeds :: IO NewsFeeds
initNewsFeeds = NewsFeeds <$> hm
  where
    hm :: IO (M.Map NewsTag (MVar RSS))
    hm = flip execStateT M.empty $ forM_ newsTags $ \tag -> do
      m <- get
      mvar <- liftIO $ newMVar $ RSS []
      put $ M.insert tag mvar m

initSearchCache :: IO SearchCache
initSearchCache = SearchCache <$> newMVar M.empty

----------------------------------------------------------------------

data Config = Config { confYahooApplicationID :: String
                     , confCrawlerDelaySec    :: Int
                     , confEnvironment        :: Environment
                     , confCrawlerEnabled     :: Bool
                     , confArticleMaxNum      :: Int
                     }
              deriving (Show)
$(deriveJSON defaultOptions{ fieldLabelModifier = drop 4 } ''Config)

readConfig :: FilePath -> IO (Maybe Config)
readConfig fp = Yaml.decodeFile fp

data Context = Context { ctxNews        :: NewsFeeds
                       , ctxSearchCache :: SearchCache
                       , ctxLogger      :: Log.Logger
                       , ctxConfig      :: Config
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

paramMaybe :: (Parsable a) => LT.Text -> Act (Maybe a)
paramMaybe key = (Just <$> param key) `rescue` (\_ -> return Nothing)

paramDefault :: (Parsable a) => LT.Text -> a -> Act a
paramDefault key defvar = do
  val <- paramMaybe key
  case val of
    Just v -> return v
    Nothing -> return defvar

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
  news <- initNewsFeeds
  scache <- initSearchCache
  logger <- Log.newLogger Log.DEBG
  conf <- fromJust <$> readConfig "./config.yml"
  let ctx = Context { ctxNews = news
                    , ctxSearchCache = scache
                    , ctxLogger = logger
                    , ctxConfig = conf
                    }
  runContextM ctx m
