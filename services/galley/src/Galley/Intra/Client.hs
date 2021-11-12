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

module Galley.Intra.Client
  ( lookupClients,
    lookupClientsFull,
    notifyClientsAboutLegalHoldRequest,
    addLegalHoldClientToUser,
    removeLegalHoldClientFromUser,
    getLegalHoldAuthToken,
  )
where

import Bilge hiding (getHeader, options, statusCode)
import Bilge.RPC
import Brig.Types.Client
import Brig.Types.Intra
import Brig.Types.Team.LegalHold (LegalHoldClientRequest (..))
import Brig.Types.User.Auth (LegalHoldLogin (..))
import Data.ByteString.Conversion (toByteString')
import Data.Id
import Data.Misc
import qualified Data.Set as Set
import Data.Text.Encoding
import Galley.API.Error
import Galley.Effects
import Galley.Env
import Galley.External.LegalHoldService.Types
import Galley.Intra.Util
import Galley.Monad
import Imports
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error hiding (Error)
import Polysemy
import Polysemy.Error
import qualified Polysemy.Reader as P
import qualified Polysemy.TinyLog as P
import qualified System.Logger.Class as Logger
import Wire.API.User.Client (UserClients, UserClientsFull, filterClients, filterClientsFull)

-- | Calls 'Brig.API.internalListClientsH'.
lookupClients :: [UserId] -> App UserClients
lookupClients uids = do
  r <-
    call Brig $
      method POST
        . path "/i/clients"
        . json (UserSet $ Set.fromList uids)
        . expect2xx
  clients <- parseResponse (mkError status502 "server-error") r
  return $ filterClients (not . Set.null) clients

-- | Calls 'Brig.API.internalListClientsFullH'.
lookupClientsFull ::
  [UserId] ->
  App UserClientsFull
lookupClientsFull uids = do
  r <-
    call Brig $
      method POST
        . path "/i/clients/full"
        . json (UserSet $ Set.fromList uids)
        . expect2xx
  clients <- parseResponse (mkError status502 "server-error") r
  return $ filterClientsFull (not . Set.null) clients

-- | Calls 'Brig.API.legalHoldClientRequestedH'.
notifyClientsAboutLegalHoldRequest ::
  UserId ->
  UserId ->
  LastPrekey ->
  App ()
notifyClientsAboutLegalHoldRequest requesterUid targetUid lastPrekey' = do
  void . call Brig $
    method POST
      . paths ["i", "clients", "legalhold", toByteString' targetUid, "request"]
      . json (LegalHoldClientRequest requesterUid lastPrekey')
      . expect2xx

-- | Calls 'Brig.User.API.Auth.legalHoldLoginH'.
getLegalHoldAuthToken ::
  Members '[Embed IO, Error InternalError, P.TinyLog, P.Reader Env] r =>
  UserId ->
  Maybe PlainTextPassword ->
  Sem r OpaqueAuthToken
getLegalHoldAuthToken uid pw = do
  r <-
    embedApp . call Brig $
      method POST
        . path "/i/legalhold-login"
        . queryItem "persist" "true"
        . json (LegalHoldLogin uid pw Nothing)
        . expect2xx
  case getCookieValue "zuid" r of
    Nothing -> do
      P.warn $ Logger.msg @Text "Response from login missing auth cookie"
      throw $ InternalErrorWithDescription "internal error"
    Just c -> pure . OpaqueAuthToken . decodeUtf8 $ c

-- | Calls 'Brig.API.addClientInternalH'.
addLegalHoldClientToUser ::
  UserId ->
  ConnId ->
  [Prekey] ->
  LastPrekey ->
  App ClientId
addLegalHoldClientToUser uid connId prekeys lastPrekey' = do
  clientId <$> brigAddClient uid connId lhClient
  where
    lhClient =
      NewClient
        prekeys
        lastPrekey'
        LegalHoldClientType
        Nothing
        (Just LegalHoldClient)
        Nothing
        Nothing
        Nothing
        Nothing

-- | Calls 'Brig.API.removeLegalHoldClientH'.
removeLegalHoldClientFromUser ::
  UserId ->
  App ()
removeLegalHoldClientFromUser targetUid = do
  void . call Brig $
    method DELETE
      . paths ["i", "clients", "legalhold", toByteString' targetUid]
      . contentJson
      . expect2xx

-- | Calls 'Brig.API.addClientInternalH'.
brigAddClient :: UserId -> ConnId -> NewClient -> App Client
brigAddClient uid connId client = do
  r <-
    call Brig $
      method POST
        . header "Z-Connection" (toByteString' connId)
        . paths ["i", "clients", toByteString' uid]
        . contentJson
        . json client
        . expect2xx
  parseResponse (mkError status502 "server-error") r
