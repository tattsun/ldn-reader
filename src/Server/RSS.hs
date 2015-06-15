module Server.RSS
       ( startCrawler
       , getRSS
       ) where

--
import           Server.Base
import           Server.RSS.Cache
import           Server.RSS.Crawler
----------------------------------------------------------------------

getRSS :: NewsTag -> ContextM RSS
getRSS = get
