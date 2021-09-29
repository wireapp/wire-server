module API.Internal
  ( tests,
  )
where

import API.Internal.Util
import Bilge
import Brig.Data.User (lookupFeatureConferenceCalling)
import qualified Brig.Options as Opt
import Brig.Types.User (userId)
import Control.Exception (ErrorCall (ErrorCall), throwIO)
import Control.Lens ((^.), (^?!))
import qualified Data.Aeson.Lens as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString.Conversion (toByteString')
import Data.Id
import qualified Data.Set as Set
import qualified Database.CQL.IO as Cass
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Util
import Util.Options (Endpoint)
import qualified Wire.API.Connection as Conn
import Wire.API.Routes.Internal.Brig.EJPD as EJPD
import qualified Wire.API.Team.Feature as ApiFt
import qualified Wire.API.Team.Member as Team

tests :: Opt.Opts -> Manager -> Cass.ClientState -> Brig -> Endpoint -> Gundeck -> Galley -> IO TestTree
tests opts mgr db brig brigep gundeck galley = do
  return $
    testGroup "api/internal" $
      [ test mgr "ejpd requests" $ testEJPDRequest mgr brig brigep gundeck,
        test mgr "account features: conferenceCalling" $ testFeatureConferenceCallingByAccount opts mgr db brig brigep galley
      ]

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
        liftIO $ assertEqual "1" (Right status) mbStatus'

        featureConfigs <- getAllFeatureConfigs galley uid
        liftIO $ assertEqual "2" status (readFeatureConfigs featureConfigs)

        featureConfigsConfCalling <- getFeatureConfig ApiFt.TeamFeatureConferenceCalling galley uid
        liftIO $ assertEqual "3" status (responseJsonUnsafe featureConfigsConfCalling)

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
        liftIO $ assertEqual "1" (Right defaultIfNull) mbStatus'

        featureConfigs <- getAllFeatureConfigs galley uid
        liftIO $ assertEqual "2" defaultIfNull (readFeatureConfigs featureConfigs)

        featureConfigsConfCalling <- getFeatureConfig ApiFt.TeamFeatureConferenceCalling galley uid
        liftIO $ assertEqual "3" defaultIfNull (responseJsonUnsafe featureConfigsConfCalling)

      readFeatureConfigs :: HasCallStack => ResponseLBS -> ApiFt.TeamFeatureStatusNoConfig
      readFeatureConfigs =
        either (error . show) id
          . Aeson.parseEither Aeson.parseJSON
          . (^?! Aeson.key "conferenceCalling")
          . responseJsonUnsafe @Aeson.Value

  check $ ApiFt.TeamFeatureStatusNoConfig ApiFt.TeamFeatureEnabled
  check $ ApiFt.TeamFeatureStatusNoConfig ApiFt.TeamFeatureDisabled
  check'

getFeatureConfig :: (MonadIO m, MonadHttp m, HasCallStack) => ApiFt.TeamFeatureName -> (Request -> Request) -> UserId -> m ResponseLBS
getFeatureConfig feature galley uid = do
  get $ galley . paths ["feature-configs", toByteString' feature] . zUser uid

getAllFeatureConfigs :: (MonadIO m, MonadHttp m, HasCallStack) => (Request -> Request) -> UserId -> m ResponseLBS
getAllFeatureConfigs galley uid = do
  get $ galley . paths ["feature-configs"] . zUser uid
