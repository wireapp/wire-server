-- Disabling to stop warnings on HasCallStack
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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

module API.Internal
  ( tests,
  )
where

import API.Internal.Util
import API.MLS.Util
import Bilge
import Bilge.Assert
import Brig.Data.User (lookupFeatureConferenceCalling, userExists)
import Brig.Options qualified as Opt
import Cassandra qualified as C
import Cassandra qualified as Cass
import Cassandra.Util
import Control.Exception (ErrorCall (ErrorCall), throwIO)
import Control.Lens ((^.), (^?!))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString.Conversion (toByteString')
import Data.Default
import Data.Id
import Data.Qualified
import GHC.TypeLits (KnownSymbol)
import Imports
import System.IO.Temp
import Test.Tasty
import Test.Tasty.HUnit
import Util
import Util.Options (Endpoint)
import Wire.API.Team.Feature
import Wire.API.Team.Feature qualified as ApiFt
import Wire.API.User
import Wire.API.User.Client

tests :: Opt.Opts -> Manager -> Cass.ClientState -> Brig -> Endpoint -> Gundeck -> Galley -> IO TestTree
tests opts mgr db brig brigep _gundeck galley = do
  pure $
    testGroup "api/internal" $
      [ test mgr "account features: conferenceCalling" $
          testFeatureConferenceCallingByAccount opts mgr db brig brigep galley,
        test mgr "suspend and unsuspend user" $ testSuspendUser db brig,
        test mgr "suspend non existing user and verify no db entry" $
          testSuspendNonExistingUser db brig,
        test mgr "mls/clients" $ testGetMlsClients brig,
        test mgr "writetimeToInt64" $ testWritetimeRepresentation opts mgr db brig brigep galley
      ]

testSuspendUser :: forall m. (TestConstraints m) => Cass.ClientState -> Brig -> m ()
testSuspendUser db brig = do
  user <- randomUser brig
  let checkAccountStatus s = do
        mbStatus <- Cass.runClient db (lookupStatus (userId user))
        liftIO $ mbStatus @?= Just s

  setAccountStatus brig (userId user) Suspended !!! const 200 === statusCode
  checkAccountStatus Suspended
  setAccountStatus brig (userId user) Active !!! const 200 === statusCode
  checkAccountStatus Active

testSuspendNonExistingUser :: forall m. (TestConstraints m) => Cass.ClientState -> Brig -> m ()
testSuspendNonExistingUser db brig = do
  nonExistingUserId <- randomId
  setAccountStatus brig nonExistingUserId Suspended !!! const 404 === statusCode
  isUserCreated <- Cass.runClient db (userExists nonExistingUserId)
  liftIO $ isUserCreated @?= False

setAccountStatus :: (MonadHttp m, HasCallStack) => Brig -> UserId -> AccountStatus -> m ResponseLBS
setAccountStatus brig u s =
  put
    ( brig
        . paths ["i", "users", toByteString' u, "status"]
        . contentJson
        . json (AccountStatusUpdate s)
    )

testFeatureConferenceCallingByAccount :: forall m. (TestConstraints m) => Opt.Opts -> Manager -> Cass.ClientState -> Brig -> Endpoint -> Galley -> m ()
testFeatureConferenceCallingByAccount (Opt.optSettings -> settings) mgr db brig brigep galley = do
  let check :: (HasCallStack) => ApiFt.WithStatusNoLock ApiFt.ConferenceCallingConfig -> m ()
      check status = do
        uid <- userId <$> createUser "joe" brig
        _ <-
          aFewTimes 12 (putAccountConferenceCallingConfigClient brigep mgr uid status) isRight
            >>= either (liftIO . throwIO . ErrorCall . ("putAccountConferenceCallingConfigClient: " <>) . show) pure

        mbStatus' <- getAccountConferenceCallingConfigClient brigep mgr uid
        liftIO $ assertEqual "GET /i/users/:uid/features/conferenceCalling" (Right status) mbStatus'

        featureConfigs <- getAllFeatureConfigs galley uid
        liftIO $ assertEqual "GET /feature-configs" status (ApiFt.forgetLock $ readFeatureConfigs featureConfigs)

        featureConfigsConfCalling <- getFeatureConfig @ApiFt.ConferenceCallingConfig galley uid
        liftIO $ assertEqual "GET /feature-configs/conferenceCalling" status (responseJsonUnsafe featureConfigsConfCalling)

      check' :: m ()
      check' = do
        uid <- userId <$> createUser "joe" brig
        let defaultIfNull :: ApiFt.WithStatus ApiFt.ConferenceCallingConfig
            defaultIfNull = settings ^. Opt.getAfcConferenceCallingDefNull

            defaultIfNewRaw :: Maybe (ApiFt.WithStatus ApiFt.ConferenceCallingConfig)
            defaultIfNewRaw =
              -- tested manually: whether we remove `defaultForNew` from `brig.yaml` or set it
              -- to `enabled` or `disabled`, this test always passes.
              settings ^. Opt.getAfcConferenceCallingDefNewMaybe

        do
          cassandraResp :: Maybe (ApiFt.WithStatusNoLock ApiFt.ConferenceCallingConfig) <-
            aFewTimes
              12
              (Cass.runClient db (lookupFeatureConferenceCalling uid))
              isJust
          liftIO $ assertEqual mempty (ApiFt.forgetLock <$> defaultIfNewRaw) cassandraResp

        _ <-
          aFewTimes 12 (deleteAccountConferenceCallingConfigClient brigep mgr uid) isRight
            >>= either (liftIO . throwIO . ErrorCall . ("deleteAccountConferenceCallingConfigClient: " <>) . show) pure

        do
          cassandraResp :: Maybe (ApiFt.WithStatusNoLock ApiFt.ConferenceCallingConfig) <-
            aFewTimes
              12
              (Cass.runClient db (lookupFeatureConferenceCalling uid))
              isJust
          liftIO $ assertEqual mempty Nothing cassandraResp

        mbStatus' <- getAccountConferenceCallingConfigClient brigep mgr uid
        liftIO $ assertEqual "GET /i/users/:uid/features/conferenceCalling" (Right (ApiFt.forgetLock defaultIfNull)) mbStatus'

        featureConfigs <- getAllFeatureConfigs galley uid
        liftIO $ assertEqual "GET /feature-configs" defaultIfNull (readFeatureConfigs featureConfigs)

        featureConfigsConfCalling <- getFeatureConfig @ApiFt.ConferenceCallingConfig galley uid
        liftIO $ assertEqual "GET /feature-configs/conferenceCalling" defaultIfNull (responseJsonUnsafe featureConfigsConfCalling)

      readFeatureConfigs :: (HasCallStack) => ResponseLBS -> ApiFt.WithStatus ApiFt.ConferenceCallingConfig
      readFeatureConfigs =
        either (error . show) id
          . Aeson.parseEither Aeson.parseJSON
          . (^?! Aeson.key "conferenceCalling")
          . responseJsonUnsafe @Aeson.Value

  check $ ApiFt.WithStatusNoLock ApiFt.FeatureStatusEnabled ApiFt.ConferenceCallingConfig ApiFt.FeatureTTLUnlimited
  check $ ApiFt.WithStatusNoLock ApiFt.FeatureStatusDisabled ApiFt.ConferenceCallingConfig ApiFt.FeatureTTLUnlimited
  check'

testGetMlsClients :: Brig -> Http ()
testGetMlsClients brig = do
  qusr <- userQualifiedId <$> randomUser brig
  c <- createClient brig qusr 0

  let getClients :: Http (Set ClientInfo)
      getClients =
        responseJsonError
          =<< get
            ( brig
                . paths ["i", "mls", "clients", toByteString' (qUnqualified qusr)]
                . queryItem "ciphersuite" "0x0001"
            )
            <!! const 200 === statusCode

  cs0 <- getClients
  liftIO $ toList cs0 @?= [ClientInfo c False]

  withSystemTempDirectory "mls" $ \tmp ->
    uploadKeyPackages brig tmp def qusr c 2

  cs1 <- getClients
  liftIO $ toList cs1 @?= [ClientInfo c True]

createClient :: Brig -> Qualified UserId -> Int -> Http ClientId
createClient brig u i =
  fmap clientId $
    responseJsonError
      =<< addClient
        brig
        (qUnqualified u)
        (defNewClient PermanentClientType [somePrekeys !! i] (someLastPrekeys !! i))
        <!! const 201 === statusCode

getFeatureConfig :: forall cfg m. (MonadHttp m, HasCallStack, KnownSymbol (ApiFt.FeatureSymbol cfg)) => (Request -> Request) -> UserId -> m ResponseLBS
getFeatureConfig galley uid = do
  get $ apiVersion "v1" . galley . paths ["feature-configs", featureNameBS @cfg] . zUser uid

getAllFeatureConfigs :: (MonadHttp m, HasCallStack) => (Request -> Request) -> UserId -> m ResponseLBS
getAllFeatureConfigs galley uid = do
  get $ galley . paths ["feature-configs"] . zUser uid

testWritetimeRepresentation :: forall m. (TestConstraints m) => Opt.Opts -> Manager -> Cass.ClientState -> Brig -> Endpoint -> Galley -> m ()
testWritetimeRepresentation _ _mgr db brig _brigep _galley = do
  quid <- userQualifiedId <$> randomUser brig
  let uid = qUnqualified quid

  ref <- fromJust <$> (runIdentity <$$> Cass.runClient db (C.query1 q1 (C.params C.LocalQuorum (Identity uid))))

  wt <- fromJust <$> (runIdentity <$$> Cass.runClient db (C.query1 q2 (C.params C.LocalQuorum (Identity uid))))

  liftIO $ assertEqual "writetimeToInt64(<fromCql WRITETIME(status)>) does not match WRITETIME(status)" ref (writetimeToInt64 wt)
  where
    q1 :: C.PrepQuery C.R (Identity UserId) (Identity Int64)
    q1 = "SELECT WRITETIME(status) from user where id = ?"

    q2 :: C.PrepQuery C.R (Identity UserId) (Identity (Writetime ()))
    q2 = "SELECT WRITETIME(status) from user where id = ?"

lookupStatus :: UserId -> C.Client (Maybe AccountStatus)
lookupStatus u =
  (runIdentity =<<)
    <$> C.retry C.x1 (C.query1 statusSelect (C.params C.LocalQuorum (Identity u)))
  where
    statusSelect :: C.PrepQuery C.R (Identity UserId) (Identity (Maybe AccountStatus))
    statusSelect = "SELECT status FROM user WHERE id = ?"
