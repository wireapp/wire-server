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

module Galley.Intra.Client
  ( lookupClients,
    lookupClientsFull,
    notifyClientsAboutLegalHoldRequest,
    addLegalHoldClientToUser,
    removeLegalHoldClientFromUser,
    getLegalHoldAuthToken,
    getClientByKeyPackageRef,
    getLocalMLSClients,
    addKeyPackageRef,
    updateKeyPackageRef,
    validateAndAddKeyPackageRef,
  )
where

import Bilge hiding (getHeader, options, statusCode)
import Bilge.RPC
import Brig.Types.Intra
import Brig.Types.Team.LegalHold (LegalHoldClientRequest (..))
import Control.Monad.Catch
import Data.ByteString.Conversion (toByteString')
import Data.Id
import Data.Misc
import Data.Qualified
import qualified Data.Set as Set
import Data.Text.Encoding
import Data.Text.Lazy (toStrict)
import Galley.API.Error
import Galley.Effects
import Galley.Env
import Galley.External.LegalHoldService.Types
import Galley.Intra.Util
import Galley.Monad
import Imports
import qualified Network.HTTP.Client as Rq
import qualified Network.HTTP.Types as HTTP
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error hiding (Error)
import qualified Network.Wai.Utilities.Error as Error
import Polysemy
import Polysemy.Error
import Polysemy.Input
import qualified Polysemy.TinyLog as P
import Servant
import qualified System.Logger.Class as Logger
import Wire.API.Error.Galley
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.Routes.Internal.Brig
import Wire.API.User.Auth.LegalHold
import Wire.API.User.Client
import Wire.API.User.Client.Prekey

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
  pure $ filterClients (not . Set.null) clients

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
  pure $ filterClientsFull (not . Set.null) clients

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
  ( Member (Embed IO) r,
    Member (Error InternalError) r,
    Member P.TinyLog r,
    Member (Input Env) r
  ) =>
  UserId ->
  Maybe PlainTextPassword6 ->
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
  App (Either AuthenticationError ClientId)
addLegalHoldClientToUser uid connId prekeys lastPrekey' = do
  fmap clientId <$> brigAddClient uid connId lhClient
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
        mempty
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
brigAddClient :: UserId -> ConnId -> NewClient -> App (Either AuthenticationError Client)
brigAddClient uid connId client = do
  r <-
    call Brig $
      method POST
        . header "Z-Connection" (toByteString' connId)
        . paths ["i", "clients", toByteString' uid]
        . contentJson
        . json client
        . expectStatus (flip elem [201, 403])
  if statusCode (responseStatus r) == 201
    then Right <$> parseResponse (mkError status502 "server-error") r
    else pure (Left ReAuthFailed)

-- | Calls 'Brig.API.Internal.getClientByKeyPackageRef'.
getClientByKeyPackageRef :: KeyPackageRef -> App (Maybe ClientIdentity)
getClientByKeyPackageRef ref = do
  r <-
    call Brig $
      method GET
        . paths ["i", "mls", "key-packages", toHeader ref]
        . expectStatus (flip elem [200, 404])
  if statusCode (responseStatus r) == 200
    then Just <$> parseResponse (mkError status502 "server-error") r
    else pure Nothing

-- | Calls 'Brig.API.Internal.getMLSClients'.
getLocalMLSClients :: Local UserId -> SignatureSchemeTag -> App (Set ClientInfo)
getLocalMLSClients lusr ss =
  call
    Brig
    ( method GET
        . paths
          [ "i",
            "mls",
            "clients",
            toByteString' (tUnqualified lusr)
          ]
        . queryItem "sig_scheme" (toByteString' (signatureSchemeName ss))
        . expect2xx
    )
    >>= parseResponse (mkError status502 "server-error")

addKeyPackageRef :: KeyPackageRef -> Qualified UserId -> ClientId -> Qualified ConvId -> App ()
addKeyPackageRef ref qusr cl qcnv =
  void $
    call
      Brig
      ( method PUT
          . paths ["i", "mls", "key-packages", toHeader ref]
          . json (NewKeyPackageRef qusr cl qcnv)
          . expect2xx
      )

updateKeyPackageRef :: KeyPackageUpdate -> App ()
updateKeyPackageRef keyPackageRef =
  void $
    call
      Brig
      ( method POST
          . paths ["i", "mls", "key-packages", toHeader $ kpupPrevious keyPackageRef]
          . json (kpupNext keyPackageRef)
          . expect2xx
      )

validateAndAddKeyPackageRef :: NewKeyPackage -> App (Either Text NewKeyPackageResult)
validateAndAddKeyPackageRef nkp = do
  res <-
    call
      Brig
      ( method PUT
          . paths ["i", "mls", "key-package-add"]
          . json nkp
      )
  let statusCode = HTTP.statusCode (Rq.responseStatus res)
  if
      | statusCode `div` 100 == 2 -> Right <$> parseResponse (mkError status502 "server-error") res
      | statusCode `div` 100 == 4 -> do
          err <- parseResponse (mkError status502 "server-error") res
          pure (Left ("Error validating keypackage: " <> toStrict (Error.label err) <> ": " <> toStrict (Error.message err)))
      | otherwise -> throwM (mkError status502 "server-error" "Unexpected http status returned from /i/mls/key-packages/add")
