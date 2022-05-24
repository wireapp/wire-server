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
import API.MLS (createClient)
import API.MLS.Util (SetKey (SetKey), uploadKeyPackages)
import Bilge
import Bilge.Assert
import Brig.Data.User (lookupFeatureConferenceCalling, lookupStatus, userExists)
import qualified Brig.Options as Opt
import Brig.Sem.UserQuery.Cassandra
import Brig.Types.Intra
import Brig.Types.User (User (userQualifiedId), userId)
import qualified Cassandra as Cass
import Control.Exception (ErrorCall (ErrorCall), throwIO)
import Control.Lens ((^.), (^?!))
import Control.Monad.Catch
import Data.Aeson (decode)
import qualified Data.Aeson.Lens as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString.Conversion (toByteString')
import Data.Id
import Data.Qualified (Qualified (qDomain, qUnqualified))
import qualified Data.Set as Set
import Imports
import Polysemy
import Servant.API (ToHttpApiData (toUrlPiece))
import Test.QuickCheck (Arbitrary (arbitrary), generate)
import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO (withSystemTempFile)
import Util
import Util.Options (Endpoint)
import qualified Wire.API.Connection as Conn
import Wire.API.MLS.KeyPackage
import Wire.API.Routes.Internal.Brig.EJPD as EJPD
import qualified Wire.API.Team.Feature as ApiFt
import qualified Wire.API.Team.Member as Team

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
        testGroup "mls/key-packages" $
          [ test mgr "fresh get" $ testKpcFreshGet brig,
            test mgr "put,get" $ testKpcPutGet brig,
            test mgr "get,get" $ testKpcGetGet brig,
            test mgr "put,put" $ testKpcPutPut brig
          ]
      ]

testSuspendUser :: forall m. TestConstraints m => Cass.ClientState -> Brig -> m ()
testSuspendUser db brig = do
  user <- randomUser brig
  let checkAccountStatus s = do
        mbStatus <- Cass.runClient db (lookupStatus (userId user))
        liftIO $ mbStatus @?= Just s

  setAccountStatus brig (userId user) Suspended !!! const 200 === statusCode
  checkAccountStatus Suspended
  setAccountStatus brig (userId user) Active !!! const 200 === statusCode
  checkAccountStatus Active

testSuspendNonExistingUser :: forall m. TestConstraints m => Cass.ClientState -> Brig -> m ()
testSuspendNonExistingUser db brig = do
  nonExistingUserId <- randomId
  setAccountStatus brig nonExistingUserId Suspended !!! const 404 === statusCode
  isUserCreated <- Cass.runClient db (runM $ userQueryToCassandra @Cass.Client @'[Embed Cass.Client] $ userExists nonExistingUserId)
  liftIO $ isUserCreated @?= False

setAccountStatus :: (MonadIO m, MonadHttp m, HasCallStack, MonadCatch m) => Brig -> UserId -> AccountStatus -> m ResponseLBS
setAccountStatus brig u s =
  put
    ( brig . paths ["i", "users", toByteString' u, "status"]
        . contentJson
        . json (AccountStatusUpdate s)
    )

testEJPDRequest :: TestConstraints m => Manager -> Brig -> Endpoint -> Gundeck -> m ()
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

testFeatureConferenceCallingByAccount :: forall m. TestConstraints m => Opt.Opts -> Manager -> Cass.ClientState -> Brig -> Endpoint -> Galley -> m ()
testFeatureConferenceCallingByAccount (Opt.optSettings -> settings) mgr db brig brigep galley = do
  let check :: HasCallStack => ApiFt.TeamFeatureStatusNoConfig -> m ()
      check status = do
        uid <- userId <$> createUser "joe" brig
        _ <-
          aFewTimes 12 (putAccountFeatureConfigClient brigep mgr uid status) isRight
            >>= either (liftIO . throwIO . ErrorCall . ("putAccountFeatureConfigClient: " <>) . show) pure

        mbStatus' <- getAccountFeatureConfigClient brigep mgr uid
        liftIO $ assertEqual "GET /i/users/:uid/features/conferenceCalling" (Right status) mbStatus'

        featureConfigs <- getAllFeatureConfigs galley uid
        liftIO $ assertEqual "GET /feature-configs" status (readFeatureConfigs featureConfigs)

        featureConfigsConfCalling <- getFeatureConfig ApiFt.TeamFeatureConferenceCalling galley uid
        liftIO $ assertEqual "GET /feature-configs/conferenceCalling" status (responseJsonUnsafe featureConfigsConfCalling)

      check' :: m ()
      check' = do
        uid <- userId <$> createUser "joe" brig
        let defaultIfNull :: ApiFt.TeamFeatureStatusNoConfig
            defaultIfNull = settings ^. Opt.getAfcConferenceCallingDefNull

            defaultIfNewRaw :: Maybe ApiFt.TeamFeatureStatusNoConfig
            defaultIfNewRaw =
              -- tested manually: whether we remove `defaultForNew` from `brig.yaml` or set it
              -- to `enabled` or `disabled`, this test always passes.
              settings ^. Opt.getAfcConferenceCallingDefNewMaybe

        do
          cassandraResp :: Maybe ApiFt.TeamFeatureStatusNoConfig <-
            aFewTimes
              12
              (Cass.runClient db (lookupFeatureConferenceCalling uid))
              isJust
          liftIO $ assertEqual mempty defaultIfNewRaw cassandraResp

        _ <-
          aFewTimes 12 (deleteAccountFeatureConfigClient brigep mgr uid) isRight
            >>= either (liftIO . throwIO . ErrorCall . ("deleteAccountFeatureConfigClient: " <>) . show) pure

        do
          cassandraResp :: Maybe ApiFt.TeamFeatureStatusNoConfig <-
            aFewTimes
              12
              (Cass.runClient db (lookupFeatureConferenceCalling uid))
              isJust
          liftIO $ assertEqual mempty Nothing cassandraResp

        mbStatus' <- getAccountFeatureConfigClient brigep mgr uid
        liftIO $ assertEqual "GET /i/users/:uid/features/conferenceCalling" (Right defaultIfNull) mbStatus'

        featureConfigs <- getAllFeatureConfigs galley uid
        liftIO $ assertEqual "GET /feature-configs" defaultIfNull (readFeatureConfigs featureConfigs)

        featureConfigsConfCalling <- getFeatureConfig ApiFt.TeamFeatureConferenceCalling galley uid
        liftIO $ assertEqual "GET /feature-configs/conferenceCalling" defaultIfNull (responseJsonUnsafe featureConfigsConfCalling)

      readFeatureConfigs :: HasCallStack => ResponseLBS -> ApiFt.TeamFeatureStatusNoConfig
      readFeatureConfigs =
        either (error . show) id
          . Aeson.parseEither Aeson.parseJSON
          . (^?! Aeson.key "conferenceCalling")
          . responseJsonUnsafe @Aeson.Value

  check $ ApiFt.TeamFeatureStatusNoConfig ApiFt.TeamFeatureEnabled
  check $ ApiFt.TeamFeatureStatusNoConfig ApiFt.TeamFeatureDisabled
  check'

keyPackageCreate :: HasCallStack => Brig -> Http KeyPackageRef
keyPackageCreate brig = do
  uid <- userQualifiedId <$> randomUser brig
  clid <- createClient brig uid 0
  withSystemTempFile "api.internal.kpc" $ \store _ ->
    uploadKeyPackages brig store SetKey uid clid 2

  uid2 <- userQualifiedId <$> randomUser brig
  claimResp <-
    post
      ( brig
          . paths
            [ "mls",
              "key-packages",
              "claim",
              toByteString' (qDomain uid),
              toByteString' (qUnqualified uid)
            ]
          . zUser (qUnqualified uid2)
          . contentJson
      )
  liftIO $
    assertEqual "POST mls/key-packages/claim/:domain/:user failed" 200 (statusCode claimResp)
  case responseBody claimResp >>= decode of
    Nothing -> liftIO $ assertFailure "Claim response empty"
    Just bundle -> case toList $ kpbEntries bundle of
      [] -> liftIO $ assertFailure "Claim response held no bundles"
      (h : _) -> pure $ kpbeRef h

kpcPut :: HasCallStack => Brig -> KeyPackageRef -> Qualified ConvId -> Http ()
kpcPut brig ref qConv = do
  resp <-
    put
      ( brig
          . paths ["i", "mls", "key-packages", toByteString' $ toUrlPiece ref, "conversation"]
          . contentJson
          . json qConv
      )
  liftIO $ assertEqual "PUT i/mls/key-packages/:ref/conversation failed" 204 (statusCode resp)

kpcGet :: HasCallStack => Brig -> KeyPackageRef -> Http (Maybe (Qualified ConvId))
kpcGet brig ref = do
  resp <-
    get (brig . paths ["i", "mls", "key-packages", toByteString' $ toUrlPiece ref, "conversation"])
  liftIO $ case statusCode resp of
    404 -> pure Nothing
    200 -> pure $ responseBody resp >>= decode
    _ -> assertFailure "GET i/mls/key-packages/:ref/conversation failed"

testKpcFreshGet :: Brig -> Http ()
testKpcFreshGet brig = do
  ref <- keyPackageCreate brig
  mqConv <- kpcGet brig ref
  liftIO $ assertEqual "(fresh) Get ~= Nothing" Nothing mqConv

testKpcPutGet :: Brig -> Http ()
testKpcPutGet brig = do
  ref <- keyPackageCreate brig
  qConv <- liftIO $ generate arbitrary
  kpcPut brig ref qConv
  mqConv <- kpcGet brig ref
  liftIO $ assertEqual "Put x; Get ~= x" (Just qConv) mqConv

testKpcGetGet :: Brig -> Http ()
testKpcGetGet brig = do
  ref <- keyPackageCreate brig
  liftIO (generate arbitrary) >>= kpcPut brig ref
  mqConv1 <- kpcGet brig ref
  mqConv2 <- kpcGet brig ref
  liftIO $ assertEqual "Get; Get ~= Get" mqConv1 mqConv2

testKpcPutPut :: Brig -> Http ()
testKpcPutPut brig = do
  ref <- keyPackageCreate brig
  qConv <- liftIO $ generate arbitrary
  qConv2 <- liftIO $ generate arbitrary
  kpcPut brig ref qConv
  kpcPut brig ref qConv2
  mqConv <- kpcGet brig ref
  liftIO $ assertEqual "Put x; Put y ~= Put y" (Just qConv2) mqConv

getFeatureConfig :: (MonadIO m, MonadHttp m, HasCallStack) => ApiFt.TeamFeatureName -> (Request -> Request) -> UserId -> m ResponseLBS
getFeatureConfig feature galley uid = do
  get $ galley . paths ["feature-configs", toByteString' feature] . zUser uid

getAllFeatureConfigs :: (MonadIO m, MonadHttp m, HasCallStack) => (Request -> Request) -> UserId -> m ResponseLBS
getAllFeatureConfigs galley uid = do
  get $ galley . paths ["feature-configs"] . zUser uid
