{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Federator.App
  ( app,
  )
where

import Data.Proxy
import qualified Federator.API as API
import Federator.Types
import Network.Wai
import Servant.API.Generic
import Servant.Mock
import Servant.Server

app :: Env -> Application
app _ = serve api (mock api Proxy)
  where
    api = Proxy @(ToServantApi API.API)
