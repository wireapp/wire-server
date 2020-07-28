{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module API.Calling where

import Bilge
import Bilge.Assert
import qualified Brig.Options as Opts
import Brig.Types
import Control.Lens ((?~), (^.), view)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as LB
import Data.Id
import Data.List ((\\))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List1 (List1)
import qualified Data.List1 as List1
import Data.Misc (Port, mkHttpsUrl)
import Imports
import Network.HTTP.Client (Manager)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
import URI.ByteString.QQ (uri)
import UnliftIO.Exception (finally)
import qualified UnliftIO.Temporary as Temp
import Util
import Wire.API.Call.Config

tests :: Manager -> Brig -> Opts.Opts -> FilePath -> FilePath -> IO TestTree
tests m b opts turn turnV2 = do
  return $
    testGroup
      "turn"
      [ test m "basic /calls/config - 200" $ testCallsConfig b,
        -- FIXME: requires tests to run on same host as brig
        test m "multiple servers /calls/config - 200" . withTurnFile turn $ testCallsConfigMultiple b,
        test m "multiple servers /calls/config/v2 - 200" . withTurnFile turnV2 $ testCallsConfigMultipleV2 b,
        test m "SFT servers /calls/config/v2 - 200" $ testSFT b opts
      ]

testCallsConfig :: Brig -> Http ()
testCallsConfig b = do
  uid <- userId <$> randomUser b
  cfg <- getTurnConfigurationV1 uid b
  let _expected = List1.singleton (toTurnURILegacy "127.0.0.1" 3478)
  assertConfiguration cfg _expected

testCallsConfigMultiple :: Brig -> TurnUpdater -> Http ()
testCallsConfigMultiple b turnUpdater = do
  uid <- userId <$> randomUser b
  -- Ensure we have a clean config
  let _expected = List1.singleton (toTurnURILegacy "127.0.0.1" 3478)
  modifyAndAssert b uid getTurnConfigurationV1 turnUpdater "turn:127.0.0.1:3478" _expected
  -- Change server list
  let _changes = "turn:127.0.0.2:3478\nturn:127.0.0.3:3478"
  let _expected =
        List1.list1
          (toTurnURILegacy "127.0.0.2" 3478)
          [toTurnURILegacy "127.0.0.3" 3478]
  modifyAndAssert b uid getTurnConfigurationV1 turnUpdater _changes _expected
  -- Change server list yet again, try adding transport and ensure that it gets dropped
  let _changes = "turn:127.0.0.2:3478?transport=udp\nturn:127.0.0.3:3478?transport=udp"
  modifyAndAssert b uid getTurnConfigurationV1 turnUpdater _changes _expected
  -- Revert the config file back to the original
  let _expected = List1.singleton (toTurnURILegacy "127.0.0.1" 3478)
  modifyAndAssert b uid getTurnConfigurationV1 turnUpdater "turn:127.0.0.1:3478" _expected

testSFT :: Brig -> Opts.Opts -> Http ()
testSFT b opts = do
  uid <- userId <$> randomUser b
  cfg <- getTurnConfigurationV2 uid b
  liftIO $
    assertEqual
      "when SFT discovery is not enabled, sft_servers shouldn't be returned"
      Nothing
      (cfg ^. rtcConfSftServers)
  withSettingsOverrides (opts & Opts.sftL ?~ Opts.SFTOptions "integration-tests.zinfra.io" Nothing) $ do
    cfg1 <- retryWhileN 10 (isJust . view rtcConfSftServers) (getTurnConfigurationV2 uid b)
    let Right server1 = mkHttpsUrl [uri|https://sft01.integration-tests.zinfra.io:443|]
    let Right server2 = mkHttpsUrl [uri|https://sft02.integration-tests.zinfra.io:8443|]
    liftIO $
      assertEqual
        "when SFT discovery is enabled, sft_servers should be returned"
        (Just (sftServer server1 :| [sftServer server2]))
        (cfg1 ^. rtcConfSftServers)

modifyAndAssert ::
  Brig ->
  UserId ->
  (UserId -> Brig -> Http RTCConfiguration) ->
  (String -> IO ()) ->
  String ->
  List1 TurnURI ->
  Http ()
modifyAndAssert b uid getTurnConfig updater newServers expected = do
  liftIO $ updater newServers
  cfg <- getTurnConfig uid b
  assertConfiguration cfg expected

testCallsConfigMultipleV2 :: Brig -> TurnUpdater -> Http ()
testCallsConfigMultipleV2 b turnUpdaterV2 = do
  uid <- userId <$> randomUser b
  -- Ensure we have a clean config
  let _expected = List1.singleton (toTurnURI SchemeTurn "localhost" 3478 Nothing)
  modifyAndAssert b uid getTurnConfigurationV2 turnUpdaterV2 "turn:localhost:3478" _expected
  -- Change server list
  let _changes = "turn:localhost:3478\nturn:localhost:3479"
  let _expected =
        List1.list1
          (toTurnURI SchemeTurn "localhost" 3478 Nothing)
          [toTurnURI SchemeTurn "localhost" 3479 Nothing]
  modifyAndAssert b uid getTurnConfigurationV2 turnUpdaterV2 _changes _expected
  -- Change server list yet again, change the transport and schema
  let _changes = "turn:localhost:3478?transport=tcp\nturns:localhost:3479?transport=tcp"
  let _expected =
        List1.list1
          (toTurnURI SchemeTurn "localhost" 3478 $ Just TransportTCP)
          [toTurnURI SchemeTurns "localhost" 3479 $ Just TransportTCP]
  modifyAndAssert b uid getTurnConfigurationV2 turnUpdaterV2 _changes _expected
  -- Ensure limit=1 returns only the udp server (see brig-types/tests for other use cases involving 'limit')
  let _changes = "turn:localhost:3478?transport=udp\nturns:localhost:3479?transport=tcp"
  let _expected2 =
        List1.list1
          (toTurnURI SchemeTurn "localhost" 3478 $ Just TransportUDP)
          [toTurnURI SchemeTurns "localhost" 3479 $ Just TransportTCP]
  let _expected1 = List1.list1 (toTurnURI SchemeTurn "localhost" 3478 $ Just TransportUDP) []
  liftIO $ turnUpdaterV2 _changes
  _cfg <- getAndValidateTurnConfigurationLimit 2 uid b
  assertConfiguration _cfg _expected2
  _cfg <- getAndValidateTurnConfigurationLimit 1 uid b
  assertConfiguration _cfg _expected1
  -- Revert the config file back to the original
  let _expected = List1.singleton (toTurnURI SchemeTurn "localhost" 3478 Nothing)
  modifyAndAssert b uid getTurnConfigurationV2 turnUpdaterV2 "turn:localhost:3478" _expected

assertConfiguration :: HasCallStack => RTCConfiguration -> List1 TurnURI -> Http ()
assertConfiguration cfg turns =
  checkIceServers (toList $ cfg ^. rtcConfIceServers) (toList turns)
  where
    -- Each turn server should _only_ be in 1 configuration
    checkIceServers :: [RTCIceServer] -> [TurnURI] -> Http ()
    checkIceServers [] [] = return ()
    checkIceServers [] ts = error ("TURN servers not advertised: " ++ show ts)
    checkIceServers (x : xs) ts = do
      remaining <- checkIceServer x ts
      checkIceServers xs remaining
    checkIceServer :: RTCIceServer -> [TurnURI] -> Http [TurnURI]
    checkIceServer srv uris = do
      let iceURIs = toList (srv ^. iceURLs)
      liftIO $ assertBool (diff iceURIs uris) (all (`elem` uris) iceURIs)
      return (uris \\ iceURIs)
      where
        diff advertised expected =
          "Some advertised URIs not expected, advertised: "
            ++ show advertised
            ++ " expected: "
            ++ show expected

getTurnConfigurationV1 :: UserId -> Brig -> Http RTCConfiguration
getTurnConfigurationV1 = getAndValidateTurnConfiguration ""

getTurnConfigurationV2 :: HasCallStack => UserId -> Brig -> ((Monad m, MonadHttp m, MonadIO m, MonadCatch m) => m RTCConfiguration)
getTurnConfigurationV2 = getAndValidateTurnConfiguration "v2"

getTurnConfiguration :: ByteString -> UserId -> Brig -> ((MonadHttp m, MonadIO m) => m (Response (Maybe LB.ByteString)))
getTurnConfiguration suffix u b =
  get
    ( b
        . paths ["/calls/config", suffix]
        . zUser u
        . zConn "conn"
    )

getAndValidateTurnConfiguration :: HasCallStack => ByteString -> UserId -> Brig -> ((Monad m, MonadIO m, MonadHttp m, MonadThrow m, MonadCatch m) => m RTCConfiguration)
getAndValidateTurnConfiguration suffix u b =
  responseJsonError =<< (getTurnConfiguration suffix u b <!! const 200 === statusCode)

getTurnConfigurationV2Limit :: Int -> UserId -> Brig -> Http (Response (Maybe LB.ByteString))
getTurnConfigurationV2Limit limit u b =
  get
    ( b
        . paths ["/calls/config/v2"]
        . zUser u
        . zConn "conn"
        . queryItem "limit" (toByteString' limit)
    )

getAndValidateTurnConfigurationLimit :: HasCallStack => Int -> UserId -> Brig -> Http RTCConfiguration
getAndValidateTurnConfigurationLimit limit u b =
  responseJsonError =<< (getTurnConfigurationV2Limit limit u b <!! const 200 === statusCode)

toTurnURILegacy :: ByteString -> Port -> TurnURI
toTurnURILegacy h p = toTurnURI SchemeTurn h p Nothing

toTurnURI :: Scheme -> ByteString -> Port -> Maybe Transport -> TurnURI
toTurnURI s h p t = turnURI s ip p t
  where
    ip =
      fromMaybe (error "Failed to parse host address") $
        fromByteString h

type TurnUpdater = String -> IO ()

withTurnFile :: FilePath -> (TurnUpdater -> Http ()) -> Http ()
withTurnFile cfgDest action = do
  Temp.withSystemTempDirectory "wire.temp" $ \tempdir -> do
    let backup = tempdir </> "backup"
    copyFile cfgDest backup
    action (setTurn tempdir cfgDest)
      `finally` copyFile backup cfgDest

-- This essentially writes 'newConf' to 'cfgDest', but in a portable way that makes sure
-- brig's filewatch notices that the file has changed.
setTurn :: FilePath -> FilePath -> String -> IO ()
setTurn tmpDir cfgDest newConf = do
  let tmpFile = tmpDir </> "file"
  writeFile tmpFile newConf
  copyFile tmpFile cfgDest
  -- TODO: This must be higher than the value specified
  -- in the watcher in Brig.App (currently, 0.5 seconds)
  threadDelay 1000000
