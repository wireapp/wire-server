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
import Control.Monad.Catch
import Data.ByteString.Conversion (toByteString')
import Data.Id
import Data.Misc
import qualified Data.Set as Set
import Data.Text.Encoding
import Galley.API.Error
import Galley.App
import Galley.Effects
import Galley.External.LegalHoldService
import Galley.Intra.Util
import Imports
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error
import qualified System.Logger.Class as Logger
import Wire.API.User.Client (UserClients, UserClientsFull, filterClients, filterClientsFull)

-- | Calls 'Brig.API.internalListClientsH'.
lookupClients :: Member BrigAccess r => [UserId] -> Galley r UserClients
lookupClients uids = do
  (brigHost, brigPort) <- brigReq
  r <-
    callBrig $
      method POST . host brigHost . port brigPort
        . path "/i/clients"
        . json (UserSet $ Set.fromList uids)
        . expect2xx
  clients <- parseResponse (mkError status502 "server-error") r
  return $ filterClients (not . Set.null) clients

-- | Calls 'Brig.API.internalListClientsFullH'.
lookupClientsFull ::
  Member BrigAccess r =>
  [UserId] ->
  Galley r UserClientsFull
lookupClientsFull uids = do
  (brigHost, brigPort) <- brigReq
  r <-
    callBrig $
      method POST . host brigHost . port brigPort
        . path "/i/clients/full"
        . json (UserSet $ Set.fromList uids)
        . expect2xx
  clients <- parseResponse (mkError status502 "server-error") r
  return $ filterClientsFull (not . Set.null) clients

-- | Calls 'Brig.API.legalHoldClientRequestedH'.
notifyClientsAboutLegalHoldRequest ::
  Member BrigAccess r =>
  UserId ->
  UserId ->
  LastPrekey ->
  Galley r ()
notifyClientsAboutLegalHoldRequest requesterUid targetUid lastPrekey' = do
  (brigHost, brigPort) <- brigReq
  void . callBrig $
    method POST
      . host brigHost
      . port brigPort
      . paths ["i", "clients", "legalhold", toByteString' targetUid, "request"]
      . json (LegalHoldClientRequest requesterUid lastPrekey')
      . expect2xx

-- | Calls 'Brig.User.API.Auth.legalHoldLoginH'.
getLegalHoldAuthToken ::
  Member BrigAccess r =>
  UserId ->
  Maybe PlainTextPassword ->
  Galley r OpaqueAuthToken
getLegalHoldAuthToken uid pw = do
  (brigHost, brigPort) <- brigReq
  r <-
    callBrig $
      method POST
        . host brigHost
        . port brigPort
        . path "/i/legalhold-login"
        . queryItem "persist" "true"
        . json (LegalHoldLogin uid pw Nothing)
        . expect2xx
  case getCookieValue "zuid" r of
    Nothing -> do
      Logger.warn $ Logger.msg @Text "Response from login missing auth cookie"
      throwM internalError
    Just c -> pure . OpaqueAuthToken . decodeUtf8 $ c

-- | Calls 'Brig.API.addClientInternalH'.
addLegalHoldClientToUser ::
  Member BrigAccess r =>
  UserId ->
  ConnId ->
  [Prekey] ->
  LastPrekey ->
  Galley r ClientId
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
  Member BrigAccess r =>
  UserId ->
  Galley r ()
removeLegalHoldClientFromUser targetUid = do
  (brigHost, brigPort) <- brigReq
  void . callBrig $
    method DELETE
      . host brigHost
      . port brigPort
      . paths ["i", "clients", "legalhold", toByteString' targetUid]
      . contentJson
      . expect2xx

-- | Calls 'Brig.API.addClientInternalH'.
brigAddClient :: Member BrigAccess r => UserId -> ConnId -> NewClient -> Galley r Client
brigAddClient uid connId client = do
  (brigHost, brigPort) <- brigReq
  r <-
    callBrig $
      method POST
        . host brigHost
        . port brigPort
        . header "Z-Connection" (toByteString' connId)
        . paths ["i", "clients", toByteString' uid]
        . contentJson
        . json client
        . expect2xx
  parseResponse (mkError status502 "server-error") r
