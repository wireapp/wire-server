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
import Bilge
import Bilge.Assert
import Brig.Data.User (lookupFeatureConferenceCalling, lookupStatus, userExists)
import Brig.Options qualified as Opt
import Cassandra qualified as C
import Cassandra qualified as Cass
import Cassandra.Util
import Control.Exception (ErrorCall (ErrorCall), throwIO)
import Control.Lens ((^.), (^?!))
import Data.Aeson.Lens qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString.Conversion (toByteString')
import Data.Id
import Data.Qualified
import Data.Set qualified as Set
import GHC.TypeLits (KnownSymbol)
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Util
import Util.Options (Endpoint)
import Wire.API.Connection qualified as Conn
import Wire.API.Routes.Internal.Brig
import Wire.API.Team.Feature
import Wire.API.Team.Feature qualified as ApiFt
import Wire.API.Team.Member qualified as Team
import Wire.API.User

tests :: Opt.Opts -> Manager -> Cass.ClientState -> Brig -> Endpoint -> Gundeck -> Galley -> IO TestTree
tests opts mgr db brig brigep gundeck galley = do
  pure $
    testGroup "api/internal" $
      [ test mgr "ejpd requests" $ testEJPDRequest mgr brig brigep gundeck,
        test mgr "account features: conferenceCalling" $
          testFeatureConferenceCallingByAccount opts mgr db brig brigep galley,
        test mgr "suspend and unsuspend user" $ testSuspendUser db brig,
        test mgr "suspend non existing user and verify no db entry" $
          testSuspendNonExistingUser db brig,
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

testEJPDRequest :: (TestConstraints m) => Manager -> Brig -> Endpoint -> Gundeck -> m ()
testEJPDRequest mgr brig brigep gundeck = do
  (handle1, mkUsr1, handle2, mkUsr2, mkUsr3) <- scaffolding brig gundeck

  do
    let req = EJPDRequestBody [handle1]
        want =
          EJPDResponseBody
            [ mkUsr1 Nothing Nothing
            ]
    have <- ejpdRequestClient brigep mgr Nothing req
    liftIO $ assertEqual "" want have

  do
    let req = EJPDRequestBody [handle1, handle2]
        want =
          EJPDResponseBody
            [ mkUsr1 Nothing Nothing,
              mkUsr2 Nothing Nothing
            ]
    have <- ejpdRequestClient brigep mgr Nothing req
    liftIO $ assertEqual "" want have

  do
    let req = EJPDRequestBody [handle2]
        want =
          EJPDResponseBody
            [ mkUsr2
                (Just (Set.fromList [(Conn.Accepted, mkUsr1 Nothing Nothing)]))
                Nothing
            ]
    have <- ejpdRequestClient brigep mgr (Just True) req
    liftIO $ assertEqual "" want have

  do
    let req = EJPDRequestBody [handle1, handle2]
        want =
          EJPDResponseBody
            [ mkUsr1
                (Just (Set.fromList [(Conn.Accepted, mkUsr2 Nothing Nothing)]))
                (Just (Set.fromList [mkUsr3 Nothing Nothing], Team.NewListComplete)),
              mkUsr2
                (Just (Set.fromList [(Conn.Accepted, mkUsr1 Nothing Nothing)]))
                Nothing
            ]
    have <- ejpdRequestClient brigep mgr (Just True) req
    liftIO $ assertEqual "" want have

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
