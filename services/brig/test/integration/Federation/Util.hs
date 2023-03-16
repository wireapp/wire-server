{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
import qualified Control.Concurrent.Async as Async
import Control.Exception (finally, throwIO)
import Control.Lens ((.~), (?~), (^.))
import Control.Monad.Catch (MonadCatch, MonadThrow, bracket, try)
import Control.Monad.Trans.Except
import Control.Retry
import Data.Aeson (FromJSON, Value, decode, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.ByteString.Conversion (toByteString')
import Data.Domain (Domain (Domain))
import Data.Handle (fromHandle)
import Data.Id
import qualified Data.Map.Strict as Map
import Data.Qualified (Qualified (..))
import Data.String.Conversions (cs)
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Database.Bloodhound as ES
import qualified Federator.MockServer as Mock
import Foreign.C.Error (Errno (..), eCONNREFUSED)
import GHC.IO.Exception (IOException (ioe_errno))
import Imports
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Media
import Network.Socket
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Test (Session)
import qualified Network.Wai.Test as WaiTest
import System.FilePath
import Test.QuickCheck (Arbitrary (arbitrary), generate)
import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ (r)
import UnliftIO (Concurrently (..), runConcurrently)
import Util
import Util.Options (Endpoint (Endpoint))
import Wire.API.Connection
import Wire.API.Conversation (Conversation (cnvMembers))
import Wire.API.Conversation.Member (OtherMember (OtherMember), cmOthers)
import Wire.API.Conversation.Role (roleNameWireAdmin)
import Wire.API.MLS.CommitBundle
import Wire.API.MLS.GroupInfoBundle
import Wire.API.MLS.Serialisation
import Wire.API.Team.Feature (FeatureStatus (..))
import Wire.API.User
import Wire.API.User.Client
import Wire.API.User.Client.Prekey

-- | Starts a server which will return the bytestring passed to this
-- function, and makes the action passed to this function run in a modified brig
-- which will contact this mocked federator instead of a real federator.
withTempMockFederator :: Opt.Opts -> LByteString -> Session a -> IO (a, [Mock.FederatedRequest])
withTempMockFederator opts resp action =
  Mock.withTempMockFederator
    [("Content-Type", "application/json")]
    (const (pure ("application" // "json", resp)))
    $ \mockPort -> do
      let opts' =
            opts
              { Opt.federatorInternal =
                  Just (Endpoint "127.0.0.1" (fromIntegral mockPort))
              }
      withSettingsOverrides opts' action

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

getConvQualified :: Galley -> UserId -> Qualified ConvId -> Http ResponseLBS
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

sendCommitBundle :: FilePath -> FilePath -> Galley -> UserId -> ClientId -> ByteString -> Http ()
sendCommitBundle tmp subGroupStateFn galley uid cid commit = do
  subGroupStateRaw <- liftIO $ BS.readFile $ tmp </> subGroupStateFn
  subGroupState <- either (liftIO . assertFailure . T.unpack) pure . decodeMLS' $ subGroupStateRaw
  subCommit <- either (liftIO . assertFailure . T.unpack) pure . decodeMLS' $ commit
  let subGroupBundle = CommitBundle subCommit Nothing (GroupInfoBundle UnencryptedGroupInfo TreeFull subGroupState)
  let subGroupBundleRaw = serializeCommitBundle subGroupBundle
  post
    ( galley
        . paths
          ["mls", "commit-bundles"]
        . zUser uid
        . zClient cid
        . zConn "conn"
        . header "Z-Type" "access"
        . content "application/x-protobuf"
        . bytes subGroupBundleRaw
    )
    !!! const 201 === statusCode
