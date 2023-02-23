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
import Data.Domain
import Data.Id
import Data.Json.Util
import Data.Qualified
import Data.Time
import Galley.API.MLS.Enabled
import Galley.API.MLS.KeyPackage
import Galley.API.Push
import Galley.Data.Conversation
import Galley.Effects.BrigAccess
import Galley.Effects.FederatorAccess
import Galley.Effects.GundeckAccess
import Galley.Env
import Imports
import qualified Network.Wai.Utilities.Error as Wai
import Network.Wai.Utilities.Server
import Polysemy
import Polysemy.Input
import qualified Polysemy.TinyLog as P
import qualified System.Logger.Class as Logger
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.API.MLS.Credential
import Wire.API.MLS.Serialisation
import Wire.API.MLS.Welcome
import Wire.API.Message

postMLSWelcome ::
  ( Member BrigAccess r,
    Member FederatorAccess r,
    Member GundeckAccess r,
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
  Member GundeckAccess r =>
  Maybe ConnId ->
  UTCTime ->
  ByteString ->
  Local [(UserId, ClientId)] ->
  Sem r ()
sendLocalWelcomes con now rawWelcome lclients = do
  runMessagePush lclients Nothing $
    foldMap (uncurry mkPush) (tUnqualified lclients)
  where
    mkPush :: UserId -> ClientId -> MessagePush 'Broadcast
    mkPush u c =
      -- FUTUREWORK: use the conversation ID stored in the key package mapping table
      let lcnv = qualifyAs lclients (selfConv u)
          lusr = qualifyAs lclients u
          e = Event (tUntagged lcnv) Nothing (tUntagged lusr) now $ EdMLSWelcome rawWelcome
       in newMessagePush lclients mempty con defMessageMetadata (u, c) e

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
      MLSWelcomeMLSNotEnabled -> logFedError x (errorToWai @'MLSNotEnabled)
    handleError (Left (r, e)) = logFedError r (toWai e)

    logFedError :: Member P.TinyLog r => Remote x -> Wai.Error -> Sem r ()
    logFedError r e =
      P.warn $
        Logger.msg ("A welcome message could not be delivered to a remote backend" :: ByteString)
          . Logger.field "remote_domain" (domainText (tDomain r))
          . logErrorMsg e
