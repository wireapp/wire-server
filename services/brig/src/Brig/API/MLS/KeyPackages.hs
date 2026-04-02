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
    claimKeyPackagesV7,
    claimLocalKeyPackages,
    countKeyPackages,
    countKeyPackagesV7,
    deleteKeyPackages,
    deleteKeyPackagesV7,
    replaceKeyPackages,
    replaceKeyPackagesV7,
  )
where

import Brig.API.Handler
import Brig.API.MLS.CipherSuite
import Brig.API.MLS.KeyPackages.Validation
import Brig.API.MLS.Util
import Brig.API.Types
import Brig.App
import Brig.Data.MLS.KeyPackage qualified as Data
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
import Wire.API.User (AccountStatus (..))
import Wire.API.User.Client
import Wire.ClientStore (ClientStore)
import Wire.ClientStore qualified as ClientStore
import Wire.ClientSubsystem.Error
import Wire.FederationAPIAccess
import Wire.GalleyAPIAccess (GalleyAPIAccess, getUserLegalholdStatus)
import Wire.GalleyAPIAccess qualified as GalleyAPIAccess
import Wire.StoredUser
import Wire.UserStore (UserStore, getUser)

uploadKeyPackages :: (Member ClientStore r) => Local UserId -> ClientId -> KeyPackageUpload -> Handler r ()
uploadKeyPackages lusr cid kps = do
  assertMLSEnabled
  let identity = mkClientIdentity (tUntagged lusr) cid
  kps' <- traverse (validateUploadedKeyPackage identity) kps.keyPackages
  lift . wrapClient $ Data.insertKeyPackages (tUnqualified lusr) cid kps'

claimKeyPackages ::
  ( Member GalleyAPIAccess r,
    Member UserStore r,
    Member ClientStore r,
    HasBrigFederationAccess m r
  ) =>
  Local UserId ->
  Maybe ClientId ->
  Qualified UserId ->
  CipherSuite ->
  Handler r KeyPackageBundle
claimKeyPackages lusr mClient target = claimKeyPackagesV7 lusr mClient target . Just

claimKeyPackagesV7 ::
  ( Member GalleyAPIAccess r,
    Member UserStore r,
    Member ClientStore r,
    HasBrigFederationAccess m r
  ) =>
  Local UserId ->
  Maybe ClientId ->
  Qualified UserId ->
  Maybe CipherSuite ->
  Handler r KeyPackageBundle
claimKeyPackagesV7 lusr mClient target mSuite = do
  assertMLSEnabled

  suite <- getCipherSuite mSuite
  foldQualified
    lusr
    (withExceptT clientErrorToHttpError . claimLocalKeyPackages (tUntagged lusr) mClient suite)
    (claimRemoteKeyPackages lusr (tagCipherSuite suite))
    target

claimLocalKeyPackages ::
  forall r.
  ( Member GalleyAPIAccess r,
    Member UserStore r,
    Member ClientStore r
  ) =>
  Qualified UserId ->
  Maybe ClientId ->
  CipherSuiteTag ->
  Local UserId ->
  ExceptT ClientError (AppT r) KeyPackageBundle
claimLocalKeyPackages qusr skipOwn suite qTarget = do
  let target = tUnqualified qTarget
  su <- lift (liftSem $ getUser target) >>= maybe (throwE (ClientUserNotFound target)) pure
  when (not su.activated || maybe True ((/=) Active) su.status) $ throwE (ClientUserNotFound target)
  -- while we do not support federation + MLS together with legalhold, to make sure that
  -- the remote backend is complicit with our legalhold policies, we disallow anyone
  -- fetching key packages for users under legalhold
  --
  -- This way we prevent both locally and on the remote to add a user under legalhold to an MLS
  -- conversation
  assertUserNotUnderLegalHold su

  -- skip own client when the target is the requesting user itself
  let own = guard (qusr == tUntagged qTarget) *> skipOwn
  clients <- map (.clientId) <$> lift (liftSem (ClientStore.lookupClients target))
  foldQualified
    qTarget
    ( \lusr ->
        lift $
          liftSem $
            GalleyAPIAccess.guardLegalHold
              (ProtectedUser (tUnqualified lusr))
              (mkUserClients [(target, clients)])
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
        uncurry (KeyPackageBundleEntry (tUntagged qTarget) c)
          <$> wrapClientM (Data.claimKeyPackage qTarget c suite)

    -- FUTUREWORK: shouldn't this be defined elsewhere for general use?
    assertUserNotUnderLegalHold :: StoredUser -> ExceptT ClientError (AppT r) ()
    assertUserNotUnderLegalHold su =
      for_ su.teamId $ \tid -> do
        mLhStatus <- lift $ liftSem $ getUserLegalholdStatus qTarget tid
        -- if an admin tries to put a user under legalhold
        -- the user has to first reject to be put under legalhold
        -- before they can join conversations again
        case ulhsrStatus <$> mLhStatus of
          Just UserLegalHoldPending -> throwE ClientLegalHoldIncompatible
          Just UserLegalHoldEnabled -> throwE ClientLegalHoldIncompatible
          Just UserLegalHoldDisabled -> pure ()
          Just UserLegalHoldNoConsent -> pure ()
          Nothing -> pure ()

claimRemoteKeyPackages ::
  ( Member ClientStore r,
    HasBrigFederationAccess m r
  ) =>
  Local UserId ->
  CipherSuite ->
  Remote UserId ->
  Handler r KeyPackageBundle
claimRemoteKeyPackages lusr suite target = do
  let req =
        ClaimKeyPackageRequest
          { claimant = tUnqualified lusr,
            target = tUnqualified target,
            cipherSuite = suite
          }
  bundle <-
    (lift $ liftSem $ runFederatedEither target $ fedClient @'Brig @"claim-key-packages" req) >>= \case
      Left e -> throwE $ clientErrorToHttpError $ ClientFederationError e
      Right Nothing -> throwE $ clientErrorToHttpError $ ClientUserNotFound (tUnqualified target)
      Right (Just bundle) -> pure bundle
  -- validate all claimed key packages
  for_ bundle.entries $ \e -> do
    let cid = mkClientIdentity e.user e.client
    kpRaw <-
      withExceptT (const . clientDataErrorToHttpError $ KeyPackageDecodingError)
        . except
        . decodeMLS'
        . kpData
        $ e.keyPackage
    (refVal, _, _) <- validateUploadedKeyPackage cid kpRaw
    unless (refVal == e.ref)
      . throwE
      . clientDataErrorToHttpError
      $ InvalidKeyPackageRef

  pure bundle

countKeyPackages :: Local UserId -> ClientId -> CipherSuite -> Handler r KeyPackageCount
countKeyPackages lusr c = countKeyPackagesV7 lusr c . Just

countKeyPackagesV7 :: Local UserId -> ClientId -> Maybe CipherSuite -> Handler r KeyPackageCount
countKeyPackagesV7 lusr c mSuite = do
  assertMLSEnabled
  suite <- getCipherSuite mSuite
  lift $
    KeyPackageCount . fromIntegral
      <$> wrapClient (Data.countKeyPackages lusr c suite)

deleteKeyPackages ::
  Local UserId ->
  ClientId ->
  CipherSuite ->
  DeleteKeyPackages ->
  Handler r ()
deleteKeyPackages lusr c = deleteKeyPackagesV7 lusr c . Just

deleteKeyPackagesV7 ::
  Local UserId ->
  ClientId ->
  Maybe CipherSuite ->
  DeleteKeyPackages ->
  Handler r ()
deleteKeyPackagesV7 lusr c mSuite (unDeleteKeyPackages -> refs) = do
  assertMLSEnabled
  suite <- getCipherSuite mSuite
  lift $ wrapClient (Data.deleteKeyPackages (tUnqualified lusr) c suite refs)

replaceKeyPackages ::
  (Member ClientStore r) =>
  Local UserId ->
  ClientId ->
  CommaSeparatedList CipherSuite ->
  KeyPackageUpload ->
  Handler r ()
replaceKeyPackages lusr c = replaceKeyPackagesV7 lusr c . Just

replaceKeyPackagesV7 ::
  (Member ClientStore r) =>
  Local UserId ->
  ClientId ->
  Maybe (CommaSeparatedList CipherSuite) ->
  KeyPackageUpload ->
  Handler r ()
replaceKeyPackagesV7 lusr c (fmap toList -> mSuites) upload = do
  assertMLSEnabled
  suites <- validateCipherSuites mSuites upload
  lift $ wrapClient (Data.deleteAllKeyPackages (tUnqualified lusr) c suites)
  uploadKeyPackages lusr c upload
