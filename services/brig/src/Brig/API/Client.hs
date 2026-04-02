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

-- TODO: Move to Brig.User.Client
module Brig.API.Client
  ( createClientDPoPAccessToken,
  )
where

import Brig.API.Types
import Brig.App
import Brig.Data.Nonce as Nonce
import Brig.Effects.JwtTools (JwtTools)
import Brig.Effects.JwtTools qualified as JwtTools
import Brig.Effects.PublicKeyBundle (PublicKeyBundle)
import Brig.Effects.PublicKeyBundle qualified as PublicKeyBundle
import Brig.Options qualified as Opt
import Control.Error
import Control.Monad.Trans.Except (except)
import Data.Bifunctor
import Data.ByteString (toStrict)
import Data.ByteString.Conversion
import Data.HavePendingInvitations
import Data.Id (ClientId, UserId)
import Data.List.Split (chunksOf)
import Data.Map.Strict qualified as Map hiding ((\\))
import Data.Qualified
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error
import Imports hiding ((\\))
import Network.HTTP.Types.Method (StdMethod)
import Network.Wai.Utilities hiding (Error)
import Polysemy
import Polysemy.Error (mapError, runError)
import Polysemy.TinyLog
import Servant (Link, ToHttpApiData (toUrlPiece))
import System.Logger.Class (field, msg, val, (~~))
import System.Logger.Class qualified as Log
import Wire.API.Component
import Wire.API.Federation.API
import Wire.API.MLS.Credential (ClientIdentity (..))
import Wire.API.MLS.Epoch (addToEpoch)
import Wire.API.Message qualified as Message
import Wire.API.Routes.Internal.Brig
import Wire.API.Team.LegalHold (LegalholdProtectee (..))
import Wire.API.User
import Wire.API.User.Client
import Wire.API.User.Client.DPoPAccessToken
import Wire.API.User.Client.Prekey
import Wire.ClientStore (ClientStore)
import Wire.ClientStore qualified as ClientStore
import Wire.ClientSubsystem
import Wire.ClientSubsystem.Error
import Wire.FederationAPIAccess
import Wire.GalleyAPIAccess
import Wire.GalleyAPIAccess qualified as GalleyAPIAccess
import Wire.Sem.Concurrency
import Wire.Sem.FromUTC (FromUTC (fromUTCTime))
import Wire.Sem.Logger as Sem.Log
import Wire.Sem.Now as Now
import Wire.UserSubsystem (UserSubsystem)
import Wire.UserSubsystem qualified as User

createClientDPoPAccessToken ::
  (Member JwtTools r, Member Now r, Member PublicKeyBundle r, Member UserSubsystem r) =>
  Local UserId ->
  ClientId ->
  StdMethod ->
  Link ->
  Proof ->
  ExceptT CertEnrollmentError (AppT r) (DPoPAccessTokenResponse, CacheControl)
createClientDPoPAccessToken luid cid method link proof = do
  let domain = tDomain luid
  let uid = tUnqualified luid
  (tid, handle, displayName) <- do
    mUser <-
      fmap listToMaybe
        . lift
        . liftSem
        . User.getAccountsBy
        . qualifyAs luid
        $ getByNoFilters {getByUserId = [tUnqualified luid], includePendingInvitations = NoPendingInvitations}
    except $
      (,,)
        <$> note NotATeamUser (userTeam =<< mUser)
        <*> note MissingHandle (userHandle =<< mUser)
        <*> note MissingName (userDisplayName <$> mUser)
  nonce <-
    ExceptT $
      note NonceNotFound
        <$> wrapClient
          ( Nonce.lookupAndDeleteNonce
              uid
              (T.decodeUtf8With lenientDecode . toStrict $ toByteString cid)
          )
  httpsUrl <-
    except $
      note MisconfiguredRequestUrl $
        fromByteString $
          "https://" <> toByteString' domain <> "/" <> T.encodeUtf8 (toUrlPiece link)
  maxSkewSeconds <- Opt.setDpopMaxSkewSecs <$> asks (.settings)
  expiresIn <- Opt.dpopTokenExpirationTimeSecs <$> asks (.settings)
  now <- fromUTCTime <$> lift (liftSem Now.get)
  let expiresAt = now & addToEpoch expiresIn
  pubKeyBundle <- do
    pathToKeys <- ExceptT (note KeyBundleError <$> asks (.settings.publicKeyBundle))
    ExceptT $ note KeyBundleError <$> liftSem (PublicKeyBundle.get pathToKeys)
  token <-
    ExceptT $
      liftSem $
        JwtTools.generateDPoPAccessToken
          proof
          (ClientIdentity domain uid cid)
          handle
          displayName
          tid
          nonce
          httpsUrl
          method
          maxSkewSeconds
          expiresAt
          pubKeyBundle
  pure $ (DPoPAccessTokenResponse token DPoP expiresIn, NoStore)
