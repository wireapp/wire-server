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
import Galley.API.MLS.KeyPackage
import Galley.API.Push
import Galley.Data.Conversation
import Galley.Effects.BrigAccess
import Galley.Effects.FederatorAccess
import Galley.Effects.GundeckAccess
import Imports
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
  Members
    '[ BrigAccess,
       FederatorAccess,
       GundeckAccess,
       ErrorS 'MLSKeyPackageRefNotFound,
       Input UTCTime,
       P.TinyLog
     ]
    r =>
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
  Members
    '[ BrigAccess,
       FederatorAccess,
       GundeckAccess,
       ErrorS 'MLSKeyPackageRefNotFound,
       Input UTCTime,
       P.TinyLog
     ]
    r =>
  Local x ->
  ConnId ->
  RawMLS Welcome ->
  Sem r ()
postMLSWelcomeFromLocalUser loc con wel = postMLSWelcome loc (Just con) wel

welcomeRecipients ::
  Members
    '[ BrigAccess,
       ErrorS 'MLSKeyPackageRefNotFound
     ]
    r =>
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
  Members '[GundeckAccess] r =>
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
          e = Event (qUntagged lcnv) (qUntagged lusr) now $ EdMLSWelcome rawWelcome
       in newMessagePush lclients mempty con defMessageMetadata (u, c) e

sendRemoteWelcomes ::
  Members
    '[ FederatorAccess,
       P.TinyLog
     ]
    r =>
  ByteString ->
  [Remote (UserId, ClientId)] ->
  Sem r ()
sendRemoteWelcomes rawWelcome clients = do
  let req = MLSWelcomeRequest . Base64ByteString $ rawWelcome
      rpc = fedClient @'Galley @"mls-welcome" req
  traverse_ handleError <=< runFederatedConcurrentlyEither clients $
    const rpc
  where
    handleError :: Member P.TinyLog r => Either (Remote [a], FederationError) x -> Sem r ()
    handleError (Right _) = pure ()
    handleError (Left (r, e)) =
      P.warn $
        Logger.msg ("A welcome message could not be delivered to a remote backend" :: ByteString)
          . Logger.field "remote_domain" (domainText (tDomain r))
          . logErrorMsg (toWai e)
