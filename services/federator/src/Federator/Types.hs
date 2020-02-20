{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Federator.Types where

import Bilge (RequestId)
import Control.Lens (makeLenses)
import Data.Metrics (Metrics)
import qualified System.Logger.Class as LC

data Env
  = Env
      { _metrics :: Metrics,
        _applog :: LC.Logger,
        _requestId :: RequestId
      }

makeLenses ''Env
