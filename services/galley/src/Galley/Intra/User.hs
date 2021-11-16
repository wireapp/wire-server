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

module Galley.Intra.User
  ( getConnections,
    getConnectionsUnqualified,
    putConnectionInternal,
    deleteBot,
    reAuthUser,
    lookupActivatedUsers,
    getUsers,
    deleteUser,
    getContactList,
    chunkify,
    getRichInfoMultiUser,
  )
where

import Bilge hiding (getHeader, options, statusCode)
import Bilge.RPC
import Brig.Types.Connection (Relation (..), UpdateConnectionsInternal (..), UserIds (..))
import qualified Brig.Types.Intra as Brig
import Brig.Types.User (User)
import Control.Monad.Catch (throwM)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Conversion
import Data.Id
import Data.Qualified
import Galley.Intra.Util
import Imports
import Network.HTTP.Client (HttpExceptionContent (..))
import qualified Network.HTTP.Client.Internal as Http
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error
import Wire.API.Routes.Internal.Brig.Connection
import Wire.API.User.RichInfo (RichInfo)

-- | Get statuses of all connections between two groups of users (the usual
-- pattern is to check all connections from one user to several, or from
-- several users to one).
--
-- When a connection does not exist, it is skipped.
-- Calls 'Brig.API.Internal.getConnectionsStatusUnqualified'.
getConnectionsUnqualified ::
  [UserId] ->
  Maybe [UserId] ->
  Maybe Relation ->
  IntraM [ConnectionStatus]
getConnectionsUnqualified uFrom uTo rlt = do
  r <-
    call Brig $
      method POST
        . path "/i/users/connections-status"
        . maybe id rfilter rlt
        . json ConnectionsStatusRequest {csrFrom = uFrom, csrTo = uTo}
        . expect2xx
  parseResponse (mkError status502 "server-error") r
  where
    rfilter = queryItem "filter" . (pack . map toLower . show)

-- | Get statuses of all connections between two groups of users (the usual
-- pattern is to check all connections from one user to several, or from
-- several users to one).
--
-- When a connection does not exist, it is skipped.
-- Calls 'Brig.API.Internal.getConnectionsStatus'.
getConnections ::
  [UserId] ->
  Maybe [Qualified UserId] ->
  Maybe Relation ->
  IntraM [ConnectionStatusV2]
getConnections [] _ _ = pure []
getConnections uFrom uTo rlt = do
  r <-
    call Brig $
      method POST
        . path "/i/users/connections-status/v2"
        . json (ConnectionsStatusRequestV2 uFrom uTo rlt)
        . expect2xx
  parseResponse (mkError status502 "server-error") r

putConnectionInternal ::
  UpdateConnectionsInternal ->
  IntraM Status
putConnectionInternal updateConn = do
  response <-
    call Brig $
      method PUT
        . paths ["/i/connections/connection-update"]
        . json updateConn
  pure $ responseStatus response

deleteBot ::
  ConvId ->
  BotId ->
  IntraM ()
deleteBot cid bot = do
  void $
    call Brig $
      method DELETE
        . path "/bot/self"
        . header "Z-Type" "bot"
        . header "Z-Bot" (toByteString' bot)
        . header "Z-Conversation" (toByteString' cid)
        . expect2xx

-- | Calls 'Brig.User.API.Auth.reAuthUserH'.
reAuthUser ::
  UserId ->
  Brig.ReAuthUser ->
  IntraM Bool
reAuthUser uid auth = do
  let req =
        method GET
          . paths ["/i/users", toByteString' uid, "reauthenticate"]
          . json auth
  st <- statusCode . responseStatus <$> call Brig (check [status200, status403] . req)
  return $ st == 200

check :: [Status] -> Request -> Request
check allowed r =
  r
    { Http.checkResponse = \rq rs ->
        when (responseStatus rs `notElem` allowed) $
          let ex = StatusCodeException (rs {responseBody = ()}) mempty
           in throwM $ HttpExceptionRequest rq ex
    }

-- | Calls 'Brig.API.listActivatedAccountsH'.
lookupActivatedUsers :: [UserId] -> IntraM [User]
lookupActivatedUsers = chunkify $ \uids -> do
  let users = BSC.intercalate "," $ toByteString' <$> uids
  r <-
    call Brig $
      method GET
        . path "/i/users"
        . queryItem "ids" users
        . expect2xx
  parseResponse (mkError status502 "server-error") r

-- | URLs with more than ~160 uids produce 400 responses, because HAProxy has a
--   URL length limit of ~6500 (determined experimentally). 100 is a
--   conservative setting. A uid contributes about 36+3 characters (+3 for the
--   comma separator) to the overall URL length.
chunkify :: forall m key a. (Monad m, ToByteString key, Monoid a) => ([key] -> m a) -> [key] -> m a
chunkify doChunk keys = mconcat <$> (doChunk `mapM` chunks keys)
  where
    maxSize :: Int
    maxSize = 100

    chunks :: [any] -> [[any]]
    chunks [] = []
    chunks uids = case splitAt maxSize uids of (h, t) -> h : chunks t

-- | Calls 'Brig.API.listActivatedAccountsH'.
getUsers :: [UserId] -> IntraM [Brig.UserAccount]
getUsers = chunkify $ \uids -> do
  resp <-
    call Brig $
      method GET
        . path "/i/users"
        . queryItem "ids" (BSC.intercalate "," (toByteString' <$> uids))
        . expect2xx
  pure . fromMaybe [] . responseJsonMaybe $ resp

-- | Calls 'Brig.API.deleteUserNoVerifyH'.
deleteUser :: UserId -> IntraM ()
deleteUser uid = do
  void $
    call Brig $
      method DELETE
        . paths ["/i/users", toByteString' uid]
        . expect2xx

-- | Calls 'Brig.API.getContactListH'.
getContactList :: UserId -> IntraM [UserId]
getContactList uid = do
  r <-
    call Brig $
      method GET
        . paths ["/i/users", toByteString' uid, "contacts"]
        . expect2xx
  cUsers <$> parseResponse (mkError status502 "server-error") r

-- | Calls 'Brig.API.Internal.getRichInfoMultiH'
getRichInfoMultiUser :: [UserId] -> IntraM [(UserId, RichInfo)]
getRichInfoMultiUser = chunkify $ \uids -> do
  resp <-
    call Brig $
      method GET
        . paths ["/i/users/rich-info"]
        . queryItem "ids" (toByteString' (List uids))
        . expect2xx
  parseResponse (mkError status502 "server-error") resp
