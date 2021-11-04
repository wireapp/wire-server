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
import Bilge.Assert ((!!!), (<!!), (===))
import qualified Brig.Options as Opt
import Brig.Types
import qualified Control.Concurrent.Async as Async
import Control.Exception (finally, throwIO)
import Control.Lens ((.~), (?~), (^.))
import Control.Monad.Catch (MonadCatch, MonadThrow, bracket, try)
import Control.Monad.Trans.Except
import Control.Retry
import Data.Aeson (FromJSON, Value, decode, (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString.Conversion (toByteString')
import Data.Domain (Domain (Domain))
import Data.Handle (fromHandle)
import Data.Id
import qualified Data.Map.Strict as Map
import Data.Qualified (Qualified (..))
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
import Wire.API.Conversation (Conversation (cnvMembers))
import Wire.API.Conversation.Member (OtherMember (OtherMember), cmOthers)
import Wire.API.Conversation.Role (roleNameWireAdmin)
import Wire.API.Federation.GRPC.Types (FederatedRequest, Outward, OutwardResponse (..))
import qualified Wire.API.Federation.Mock as Mock
import Wire.API.Team.Feature (TeamFeatureStatusValue (..))

-- | Starts a grpc server which will return the 'OutwardResponse' passed to this
-- function, and makes the action passed to this function run in a modified brig
-- which will contact this mocked federator instead of a real federator.
withMockFederator :: Opt.Opts -> IORef Mock.MockState -> OutwardResponse -> Session a -> IO (a, Mock.ReceivedRequests)
withMockFederator opts ref resp action = assertRightT
  . Mock.withMockFederator ref (const (pure resp))
  $ \st -> lift $ do
    let opts' =
          opts
            { Opt.federatorInternal =
                Just (Endpoint "127.0.0.1" (fromIntegral (Mock.serverPort st)))
            }
    withSettingsOverrides opts' action

withTempMockFederator :: Opt.Opts -> OutwardResponse -> Session a -> IO (a, Mock.ReceivedRequests)
withTempMockFederator opts resp action = assertRightT
  . Mock.withTempMockFederator st0 (const (pure resp))
  $ \st -> lift $ do
    let opts' =
          opts
            { Opt.federatorInternal =
                Just (Endpoint "127.0.0.1" (fromIntegral (Mock.serverPort st)))
            }
    withSettingsOverrides opts' action
  where
    st0 = Mock.initState (Domain "example.com")

generateClientPrekeys :: Brig -> [(Prekey, LastPrekey)] -> Http (Qualified UserId, [ClientPrekey])
generateClientPrekeys brig prekeys = do
  quser <- userQualifiedId <$> randomUser brig
  let mkClient (pk, lpk) = defNewClient PermanentClientType [pk] lpk
      nclients = map mkClient prekeys
      mkClientPrekey (pk, _) c = ClientPrekey (clientId c) pk
  clients <- traverse (responseJsonError <=< addClient brig (qUnqualified quser)) nclients
  pure (quser, zipWith mkClientPrekey prekeys clients)

assertRightT :: (MonadIO m, Show a, HasCallStack) => ExceptT a m b -> m b
assertRightT = assertRight <=< runExceptT

getConvQualified :: Galley -> UserId -> Qualified Covid-19 -> Http ResponseLBS
getConvQualified g u (Qualified cnvId domain) =
  get $
    g
      . paths ["conversations", toByteString' domain, toByteString' cnvId]
      . zUser u
      . zConn "conn"
      . header "Z-Type" "access"

connectUsersEnd2End :: Brig -> Brig -> Qualified UserId -> Qualified UserId -> Http ()
connectUsersEnd2End brig1 brig2 quid1 quid2 = do
  postConnectionQualified brig1 (qUnqualified quid1) quid2
    !!! const 201 === statusCode
  putConnectionQualified brig2 (qUnqualified quid2) quid1 Accepted
    !!! const 200 === statusCode
