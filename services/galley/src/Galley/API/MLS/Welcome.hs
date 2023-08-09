{-# OPTIONS -Wno-redundant-constraints#-}

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

module Galley.API.MLS.Welcome
  ( postMLSWelcome,
    postMLSWelcomeFromLocalUser,
    sendLocalWelcomes,
  )
where

import Control.Comonad
import Data.Aeson qualified as A
import Data.Domain
import Data.Id
import Data.Json.Util
import Data.Qualified
import Data.Time
import Galley.API.MLS.Enabled
import Galley.API.MLS.KeyPackage
import Galley.Effects.BrigAccess
import Galley.Effects.ExternalAccess
import Galley.Effects.FederatorAccess
import Galley.Effects.GundeckAccess
import Galley.Env
import Imports
import Network.Wai.Utilities.JSONResponse
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog qualified as P
import System.Logger.Class qualified as Logger
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.API.MLS.Credential
import Wire.API.MLS.Serialisation
import Wire.API.MLS.Welcome

postMLSWelcome ::
  ( Member BrigAccess r,
    Member FederatorAccess r,
    Member GundeckAccess r,
    Member ExternalAccess r,
    Member (ErrorS 'MLSKeyPackageRefNotFound) r,
    Member (Input UTCTime) r,
    Member P.TinyLog r
  ) =>
  Local x ->
  Maybe ConnId ->
  RawMLS Welcome ->
  Sem r ()
postMLSWelcome loc con wel = do
  now <- input
  rcpts <- welcomeRecipients (rmValue wel)
  let (locals, remotes) = partitionQualified loc rcpts
  sendLocalWelcomes con now (rmRaw wel) (qualifyAs loc locals)
  sendRemoteWelcomes (rmRaw wel) remotes

postMLSWelcomeFromLocalUser ::
  ( Member BrigAccess r,
    Member FederatorAccess r,
    Member GundeckAccess r,
    Member ExternalAccess r,
    Member (ErrorS 'MLSKeyPackageRefNotFound) r,
    Member (ErrorS 'MLSNotEnabled) r,
    Member (Input UTCTime) r,
    Member (Input Env) r,
    Member P.TinyLog r
  ) =>
  Local x ->
  ConnId ->
  RawMLS Welcome ->
  Sem r ()
postMLSWelcomeFromLocalUser loc con wel = do
  assertMLSEnabled
  postMLSWelcome loc (Just con) wel

welcomeRecipients ::
  ( Member BrigAccess r,
    Member (ErrorS 'MLSKeyPackageRefNotFound) r
  ) =>
  Welcome ->
  Sem r [Qualified (UserId, ClientId)]
welcomeRecipients =
  traverse
    ( fmap cidQualifiedClient
        . derefKeyPackage
        . gsNewMember
    )
    . welSecrets

sendLocalWelcomes ::
  Maybe ConnId ->
  UTCTime ->
  ByteString ->
  Local [(UserId, ClientId)] ->
  Sem r ()
sendLocalWelcomes _con _now _rawWelcome _lclients = do
  -- This function is only implemented on the MLS branch.
  pure ()

sendRemoteWelcomes ::
  ( Member FederatorAccess r,
    Member P.TinyLog r
  ) =>
  ByteString ->
  [Remote (UserId, ClientId)] ->
  Sem r ()
sendRemoteWelcomes rawWelcome clients = do
  let req = MLSWelcomeRequest . Base64ByteString $ rawWelcome
      rpc = fedClient @'Galley @"mls-welcome" req
  traverse_ handleError <=< runFederatedConcurrentlyEither clients $
    const rpc
  where
    handleError ::
      Member P.TinyLog r =>
      Either (Remote [a], FederationError) (Remote MLSWelcomeResponse) ->
      Sem r ()
    handleError (Right x) = case tUnqualified x of
      MLSWelcomeSent -> pure ()
      MLSWelcomeMLSNotEnabled -> logFedError x (errorToResponse @'MLSNotEnabled)
    handleError (Left (r, e)) = logFedError r (toResponse e)

    logFedError :: Member P.TinyLog r => Remote x -> JSONResponse -> Sem r ()
    logFedError r e =
      P.warn $
        Logger.msg ("A welcome message could not be delivered to a remote backend" :: ByteString)
          . Logger.field "remote_domain" (domainText (tDomain r))
          . Logger.field "error" (A.encode e.value)
