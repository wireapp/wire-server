-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Proxy.Proxy
  ( Proxy,
    runProxy,
  )
where

import Bilge.Request (requestIdName)
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.IO.Unlift ()
import Data.Default (def)
import Data.Id (RequestId (..))
import Imports
import Network.Wai
import Proxy.Env
import qualified System.Logger as Logger
import System.Logger.Class hiding (Error, info)

newtype Proxy a
  = Proxy
      { unProxy :: ReaderT Env IO a
      }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadReader Env,
      MonadUnliftIO
    )

instance MonadLogger Proxy where
  log l m = ask >>= \e -> Logger.log (e ^. applog) l (reqIdMsg (e ^. reqId) . m)

runProxy :: Env -> Request -> Proxy ResponseReceived -> IO ResponseReceived
runProxy e r m = runReaderT (unProxy m) (reqId .~ lookupReqId r $ e)

reqIdMsg :: RequestId -> Msg -> Msg
reqIdMsg = ("request" .=) . unRequestId
{-# INLINE reqIdMsg #-}

lookupReqId :: Request -> RequestId
lookupReqId = maybe def RequestId . lookup requestIdName . requestHeaders
