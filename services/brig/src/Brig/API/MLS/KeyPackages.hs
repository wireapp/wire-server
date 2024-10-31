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

module Brig.API.MLS.KeyPackages
  ( uploadKeyPackages,
    claimKeyPackages,
    claimLocalKeyPackages,
    countKeyPackages,
    deleteKeyPackages,
    replaceKeyPackages,
  )
where

import Brig.API.Error
import Brig.API.Handler
import Brig.API.MLS.CipherSuite
import Brig.API.MLS.KeyPackages.Validation
import Brig.API.MLS.Util
import Brig.API.Types
import Brig.App
import Brig.Data.Client qualified as Data
import Brig.Data.MLS.KeyPackage qualified as Data
import Brig.Federation.Client
import Brig.IO.Intra
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.CommaSeparatedList
import Data.Id
import Data.LegalHold
import Data.Qualified
import Data.Set qualified as Set
import Imports
import Polysemy (Member)
import Wire.API.Federation.API
import Wire.API.Federation.API.Brig
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Serialisation
import Wire.API.Team.LegalHold
import Wire.API.User.Client
import Wire.GalleyAPIAccess (GalleyAPIAccess, getUserLegalholdStatus)
import Wire.StoredUser
import Wire.UserStore (UserStore, getUser)

uploadKeyPackages :: Local UserId -> ClientId -> KeyPackageUpload -> Handler r ()
uploadKeyPackages lusr cid kps = do
  assertMLSEnabled
  let identity = mkClientIdentity (tUntagged lusr) cid
  kps' <- traverse (validateUploadedKeyPackage identity) kps.keyPackages
  lift . wrapClient $ Data.insertKeyPackages (tUnqualified lusr) cid kps'

claimKeyPackages ::
  ( Member GalleyAPIAccess r,
    Member UserStore r
  ) =>
  Local UserId ->
  Maybe ClientId ->
  Qualified UserId ->
  Maybe CipherSuite ->
  Handler r KeyPackageBundle
claimKeyPackages lusr mClient target mSuite = do
  assertMLSEnabled

  suite <- getCipherSuite mSuite
  foldQualified
    lusr
    (withExceptT clientError . claimLocalKeyPackages (tUntagged lusr) mClient suite)
    (claimRemoteKeyPackages lusr (tagCipherSuite suite))
    target

claimLocalKeyPackages ::
  forall r.
  ( Member GalleyAPIAccess r,
    Member UserStore r
  ) =>
  Qualified UserId ->
  Maybe ClientId ->
  CipherSuiteTag ->
  Local UserId ->
  ExceptT ClientError (AppT r) KeyPackageBundle
claimLocalKeyPackages qusr skipOwn suite target = do
  -- while we do not support federation + MLS together with legalhold, to make sure that
  -- the remote backend is complicit with our legalhold policies, we disallow anyone
  -- fetching key packages for users under legalhold
  --
  -- This way we prevent both locally and on the remote to add a user under legalhold to an MLS
  -- conversation
  assertUserNotUnderLegalHold

  -- skip own client when the target is the requesting user itself
  let own = guard (qusr == tUntagged target) *> skipOwn
  clients <- map clientId <$> wrapClientE (Data.lookupClients (tUnqualified target))
  foldQualified
    target
    ( \lusr ->
        guardLegalhold
          (ProtectedUser (tUnqualified lusr))
          (mkUserClients [(tUnqualified target, clients)])
    )
    (\_ -> pure ())
    qusr
  lift $
    KeyPackageBundle . Set.fromList . catMaybes <$> traverse (mkEntry own) clients
  where
    mkEntry :: Maybe ClientId -> ClientId -> AppT r (Maybe KeyPackageBundleEntry)
    mkEntry own c =
      runMaybeT $ do
        guard $ Just c /= own
        uncurry (KeyPackageBundleEntry (tUntagged target) c)
          <$> wrapClientM (Data.claimKeyPackage target c suite)

    assertUserNotUnderLegalHold :: ExceptT ClientError (AppT r) ()
    assertUserNotUnderLegalHold = do
      -- this is okay because there can only be one StoredUser per UserId
      mSu <- lift $ liftSem $ getUser (tUnqualified target)
      case mSu of
        Nothing -> pure () -- Legalhold is a team feature.
        Just su ->
          for_ su.teamId $ \tid -> do
            resp <- lift $ liftSem $ getUserLegalholdStatus target tid
            -- if an admin tries to put a user under legalhold
            -- the user has to first reject to be put under legalhold
            -- before they can join conversations again
            case resp.ulhsrStatus of
              UserLegalHoldPending -> throwE ClientLegalHoldIncompatible
              UserLegalHoldEnabled -> throwE ClientLegalHoldIncompatible
              UserLegalHoldDisabled -> pure ()
              UserLegalHoldNoConsent -> pure ()

claimRemoteKeyPackages ::
  Local UserId ->
  CipherSuite ->
  Remote UserId ->
  Handler r KeyPackageBundle
claimRemoteKeyPackages lusr suite target = do
  bundle <-
    withExceptT clientError
      . (handleFailure =<<)
      $ withExceptT ClientFederationError
      $ runBrigFederatorClient (tDomain target)
      $ fedClient @'Brig @"claim-key-packages"
      $ ClaimKeyPackageRequest
        { claimant = tUnqualified lusr,
          target = tUnqualified target,
          cipherSuite = suite
        }

  -- validate all claimed key packages
  for_ bundle.entries $ \e -> do
    let cid = mkClientIdentity e.user e.client
    kpRaw <-
      withExceptT (const . clientDataError $ KeyPackageDecodingError)
        . except
        . decodeMLS'
        . kpData
        $ e.keyPackage
    (refVal, _, _) <- validateUploadedKeyPackage cid kpRaw
    unless (refVal == e.ref)
      . throwE
      . clientDataError
      $ InvalidKeyPackageRef

  pure bundle
  where
    handleFailure :: (Monad m) => Maybe x -> ExceptT ClientError m x
    handleFailure = maybe (throwE (ClientUserNotFound (tUnqualified target))) pure

countKeyPackages :: Local UserId -> ClientId -> Maybe CipherSuite -> Handler r KeyPackageCount
countKeyPackages lusr c mSuite = do
  assertMLSEnabled
  suite <- getCipherSuite mSuite
  lift $
    KeyPackageCount . fromIntegral
      <$> wrapClient (Data.countKeyPackages lusr c suite)

deleteKeyPackages ::
  Local UserId ->
  ClientId ->
  Maybe CipherSuite ->
  DeleteKeyPackages ->
  Handler r ()
deleteKeyPackages lusr c mSuite (unDeleteKeyPackages -> refs) = do
  assertMLSEnabled
  suite <- getCipherSuite mSuite
  lift $ wrapClient (Data.deleteKeyPackages (tUnqualified lusr) c suite refs)

replaceKeyPackages ::
  Local UserId ->
  ClientId ->
  Maybe (CommaSeparatedList CipherSuite) ->
  KeyPackageUpload ->
  Handler r ()
replaceKeyPackages lusr c (fmap toList -> mSuites) upload = do
  assertMLSEnabled
  suites <- validateCipherSuites mSuites upload
  lift $ wrapClient (Data.deleteAllKeyPackages (tUnqualified lusr) c suites)
  uploadKeyPackages lusr c upload
