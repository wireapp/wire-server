{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

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

module Federation.Util where

import Bilge
import qualified Brig.Options as Opt
import Brig.Types
import qualified Control.Concurrent.Async as Async
import Control.Exception (finally, throwIO)
import Control.Lens ((.~), (?~), (^.))
import Control.Monad.Catch (MonadCatch, MonadThrow, bracket, try)
import Control.Retry
import Data.Aeson (FromJSON, Value, decode, (.=))
import qualified Data.Aeson as Aeson
import Data.Domain (Domain (Domain))
import Data.Handle (fromHandle)
import Data.Id
import qualified Data.Map.Strict as Map
import Data.Qualified (Qualified (qDomain, qUnqualified))
import Data.String.Conversions (cs)
import qualified Data.Text as Text
import qualified Database.Bloodhound as ES
import Foreign.C.Error (Errno (..), eCONNREFUSED)
import GHC.IO.Exception (IOException (ioe_errno))
import qualified Galley.Types.Teams.SearchVisibility as Team
import Imports
import Mu.GRpc.Server (msgProtoBuf, runGRpcApp)
import Mu.Server (ServerErrorIO, SingleServerT)
import qualified Mu.Server as Mu
import qualified Network.HTTP.Client as HTTP
import Network.Socket
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Test (Session)
import qualified Network.Wai.Test as WaiTest
import Test.QuickCheck (Arbitrary (arbitrary), generate)
import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ (r)
import UnliftIO (Concurrently (..), runConcurrently)
import Util
import Util.Options (Endpoint (Endpoint))
import Wire.API.Federation.GRPC.Types (FederatedRequest, HTTPResponse (HTTPResponse), Outward, OutwardResponse (OutwardResponseError, OutwardResponseHTTPResponse))
import Wire.API.Team.Feature (TeamFeatureStatusValue (..))

-- | Starts a grpc server which will return the 'OutwardResponse' passed to this
-- function, and makes the action passed to this function run in a modified brig
-- which will contact this mocked federator instead of a real federator.
withMockFederator :: forall (m :: * -> *) a. MonadIO m => Opt.Opts -> Port -> OutwardResponse -> Session a -> m a
withMockFederator opts p res action = do
  federatorThread <- liftIO . Async.async $ runGRpcApp msgProtoBuf p (outwardService res)
  void $ retryWhileN 5 id isPortOpen
  liftIO $
    (withSettingsOverrides newOpts action)
      `finally` (Async.cancel federatorThread)
  where
    newOpts :: Opt.Opts
    newOpts = opts {Opt.federatorInternal = Just (Endpoint "127.0.0.1" (fromIntegral p))}

    isPortOpen :: m Bool
    isPortOpen = liftIO $ do
      let sockAddr = SockAddrInet (fromIntegral p) (tupleToHostAddress (127, 0, 0, 1))
      bracket (socket AF_INET Stream 6 {- TCP -}) close' $ \sock -> do
        portRes <- try $ connect sock sockAddr
        case portRes of
          Right () -> return True
          Left e ->
            if (Errno <$> ioe_errno e) == Just eCONNREFUSED
              then return False
              else throwIO e

outwardService :: OutwardResponse -> SingleServerT info Outward ServerErrorIO _
outwardService response = Mu.singleService (Mu.method @"call" (callOutward response))

callOutward :: OutwardResponse -> FederatedRequest -> ServerErrorIO OutwardResponse
callOutward res _ = pure res
