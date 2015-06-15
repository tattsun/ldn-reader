{-# LANGUAGE OverloadedStrings #-}
module Server.RSS.Cache where

import           Control.Concurrent
--
import           Server.Base
----------------------------------------------------------------------

get :: NewsTag -> ContextM RSS
get = undefined

put :: NewsTag -> RSS -> ContextM ()
put = undefined
