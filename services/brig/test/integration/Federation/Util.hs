{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

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
import Bilge.Assert ((!!!), (===))
import qualified Brig.Options as Opt
import Control.Monad.Trans.Except
import Data.ByteString.Conversion (toByteString')
import Data.Id
import Data.Qualified (Qualified (..))
import qualified Federator.MockServer as Mock
import Imports
import Network.HTTP.Media
import Network.Wai.Test (Session)
import Util
import Util.Options (Endpoint (Endpoint))
import Wire.API.Connection
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
