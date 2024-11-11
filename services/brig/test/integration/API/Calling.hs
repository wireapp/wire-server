{-# LANGUAGE DeepSubsumption #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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

module API.Calling where

import Bilge
import Bilge.Assert
import Control.Lens (view, (.~), (?~), (^.))
import Control.Monad.Catch (MonadCatch)
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString.Conversion
import Data.ByteString.Lazy qualified as LB
import Data.Id
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Misc (Port (..), mkHttpsUrl)
import Data.Set qualified as Set
import Data.String.Conversions
import Imports
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
import URI.ByteString (laxURIParserOptions, parseURI)
import UnliftIO.Exception (finally)
import UnliftIO.Temporary qualified as Temp
import Util
import Wire.API.Call.Config
import Wire.API.User
import Wire.ServerOptions.Brig qualified as Opts

tests :: Manager -> Brig -> Opts.Opts -> FilePath -> FilePath -> IO TestTree
tests m b opts turn turnV2 = do
  pure $
    testGroup "calling" $
      [ testGroup "turn" $
          [ test m "basic /calls/config - 200" $ testCallsConfig b,
            -- FIXME: requires tests to run on same host as brig
            test m "multiple servers using files /calls/config - 200" . withTurnFile turn $ testCallsConfigMultiple b,
            test m "multiple servers using SRV /calls/config - 200" $ testCallsConfigSRV b opts,
            test m "multiple servers using files /calls/config/v2 - 200" . withTurnFile turnV2 $ testCallsConfigMultipleV2 b,
            test m "multiple servers using SRV records /calls/config/v2 - 200" $ testCallsConfigV2SRV b opts
          ],
        testGroup
          "sft"
          [ test m "SFT servers /calls/config/v2 - 200" $ testSFT b opts,
            test m "SFT servers /calls/config/v2 - 200 - SFT does not respond as expected" $ testSFTUnavailable b opts "https://example.com",
            test m "SFT servers /calls/config/v2 - 200 - SFT DNS does not resolve" $ testSFTUnavailable b opts "https://sft.example.com"
          ]
      ]

testCallsConfig :: Brig -> Http ()
testCallsConfig b = do
  uid <- userId <$> randomUser b
  cfg <- getTurnConfigurationV1 uid b
  let _expected = toTurnURILegacy "127.0.0.1" 3478 :| []
  assertConfiguration cfg _expected

testCallsConfigMultiple :: Brig -> TurnUpdater -> Http ()
testCallsConfigMultiple b turnUpdater = do
  uid <- userId <$> randomUser b
  -- Ensure we have a clean config
  let _expected = toTurnURILegacy "127.0.0.1" 3478 :| []
  modifyAndAssert b uid getTurnConfigurationV1 turnUpdater "turn:127.0.0.1:3478" _expected
  -- Change server list
  let _changes = "turn:127.0.0.2:3478\nturn:127.0.0.3:3478"
  let _expected =
        toTurnURILegacy "127.0.0.2" 3478
          :| [toTurnURILegacy "127.0.0.3" 3478]
  modifyAndAssert b uid getTurnConfigurationV1 turnUpdater _changes _expected
  -- Change server list yet again, try adding transport and ensure that it gets dropped
  let _changes = "turn:127.0.0.2:3478?transport=udp\nturn:127.0.0.3:3478?transport=udp"
  modifyAndAssert b uid getTurnConfigurationV1 turnUpdater _changes _expected
  -- Revert the config file back to the original
  let _expected = toTurnURILegacy "127.0.0.1" 3478 :| []
  modifyAndAssert b uid getTurnConfigurationV1 turnUpdater "turn:127.0.0.1:3478" _expected

-- | This test relies on pre-created public DNS records. Code here:
-- https://github.com/zinfra/cailleach/blob/fb4caacaca02e6e28d68dc0cdebbbc987f5e31da/targets/misc/wire-server-integration-tests/dns.tf
testSFT :: Brig -> Opts.Opts -> Http ()
testSFT b opts = do
  uid <- userId <$> randomUser b
  cfg <- getTurnConfigurationV2 uid b
  liftIO $ do
    assertEqual
      "when SFT discovery is not enabled, sft_servers shouldn't be returned"
      Nothing
      (cfg ^. rtcConfSftServers)
  withSettingsOverrides (opts & Opts.sftLens ?~ Opts.SFTOptions "integration-tests.zinfra.io" Nothing (Just 0.001) Nothing Nothing) $ do
    cfg1 <- retryWhileN 10 (isNothing . view rtcConfSftServers) (getTurnConfigurationV2 uid b)
    -- These values are controlled by https://github.com/zinfra/cailleach/tree/77ca2d23cf2959aa183dd945d0a0b13537a8950d/environments/dns-integration-tests
    let Right server1 = mkHttpsUrl =<< first show (parseURI laxURIParserOptions "https://sft01.integration-tests.zinfra.io:443")
    let Right server2 = mkHttpsUrl =<< first show (parseURI laxURIParserOptions "https://sft02.integration-tests.zinfra.io:8443")
    liftIO $
      assertEqual
        "when SFT discovery is enabled, sft_servers should be returned"
        (Set.fromList [sftServer server1, sftServer server2])
        (Set.fromList $ maybe [] NonEmpty.toList $ cfg1 ^. rtcConfSftServers)

testSFTUnavailable :: Brig -> Opts.Opts -> String -> Http ()
testSFTUnavailable b opts domain = do
  uid <- userId <$> randomUser b
  withSettingsOverrides (opts {Opts.settings = (Opts.settings opts) {Opts.sftStaticUrl = fromByteString (cs domain), Opts.sftListAllServers = Just Opts.ListAllSFTServers}}) $ do
    cfg <- getTurnConfigurationV2 uid b
    liftIO $ do
      assertEqual
        "sft_servers_all should be missing"
        Nothing
        (cfg ^. rtcConfSftServersAll)

modifyAndAssert ::
  (HasCallStack) =>
  Brig ->
  UserId ->
  (UserId -> Brig -> Http RTCConfiguration) ->
  (String -> IO ()) ->
  String ->
  NonEmpty TurnURI ->
  Http ()
modifyAndAssert b uid getTurnConfig updater newServers expected = do
  liftIO $ updater newServers
  cfg <- getTurnConfig uid b
  assertConfiguration cfg expected

testCallsConfigMultipleV2 :: Brig -> TurnUpdater -> Http ()
testCallsConfigMultipleV2 b turnUpdaterV2 = do
  uid <- userId <$> randomUser b
  -- Ensure we have a clean config
  let _expected = toTurnURI SchemeTurn "localhost" 3478 Nothing :| []
  modifyAndAssert b uid getTurnConfigurationV2 turnUpdaterV2 "turn:localhost:3478" _expected
  -- Change server list
  let _changes = "turn:localhost:3478\nturn:localhost:3479"
  let _expected =
        toTurnURI SchemeTurn "localhost" 3478 Nothing
          :| [toTurnURI SchemeTurn "localhost" 3479 Nothing]
  modifyAndAssert b uid getTurnConfigurationV2 turnUpdaterV2 _changes _expected
  -- Change server list yet again, change the transport and schema
  let _changes = "turn:localhost:3478?transport=tcp\nturns:localhost:3479?transport=tcp"
  let _expected =
        toTurnURI SchemeTurn "localhost" 3478 (Just TransportTCP)
          :| [toTurnURI SchemeTurns "localhost" 3479 $ Just TransportTCP]
  modifyAndAssert b uid getTurnConfigurationV2 turnUpdaterV2 _changes _expected
  -- Ensure limit=1 returns only the udp server (see brig-types/tests for other use cases involving 'limit')
  let _changes = "turn:localhost:3478?transport=udp\nturns:localhost:3479?transport=tcp"
  let _expected2 =
        toTurnURI SchemeTurn "localhost" 3478 (Just TransportUDP)
          :| [toTurnURI SchemeTurns "localhost" 3479 $ Just TransportTCP]
  let _expected1 = toTurnURI SchemeTurn "localhost" 3478 (Just TransportUDP) :| []
  liftIO $ turnUpdaterV2 _changes
  _cfg <- getAndValidateTurnConfigurationLimit 2 uid b
  assertConfiguration _cfg _expected2
  _cfg <- getAndValidateTurnConfigurationLimit 1 uid b
  assertConfiguration _cfg _expected1
  -- Revert the config file back to the original
  let _expected = toTurnURI SchemeTurn "localhost" 3478 Nothing :| []
  modifyAndAssert b uid getTurnConfigurationV2 turnUpdaterV2 "turn:localhost:3478" _expected

-- | This test relies on pre-created public DNS records. Code here:
-- https://github.com/zinfra/cailleach/blob/fb4caacaca02e6e28d68dc0cdebbbc987f5e31da/targets/misc/wire-server-integration-tests/dns.tf
testCallsConfigSRV :: Brig -> Opts.Opts -> Http ()
testCallsConfigSRV b opts = do
  uid <- userId <$> randomUser b
  let dnsOpts = Opts.TurnSourceDNS (Opts.TurnDnsOpts "integration-tests.zinfra.io" (Just 0.5))
  config <-
    withSettingsOverrides (opts & Opts.turnLens . Opts.serversSourceLens .~ dnsOpts) $
      responseJsonError
        =<< ( retryWhileN 10 (\r -> statusCode r /= 200) (getTurnConfiguration "" uid b)
                <!! const 200 === statusCode
            )
  assertConfiguration
    config
    ( toTurnURI SchemeTurn "127.0.0.27" 3479 Nothing
        :| [toTurnURI SchemeTurn "127.0.0.27" 3478 Nothing]
    )

-- | This test relies on pre-created public DNS records. Code here:
-- https://github.com/zinfra/cailleach/blob/fb4caacaca02e6e28d68dc0cdebbbc987f5e31da/targets/misc/wire-server-integration-tests/dns.tf
testCallsConfigV2SRV :: Brig -> Opts.Opts -> Http ()
testCallsConfigV2SRV b opts = do
  uid <- userId <$> randomUser b
  let dnsOpts = Opts.TurnSourceDNS (Opts.TurnDnsOpts "integration-tests.zinfra.io" (Just 0.5))
  config <-
    withSettingsOverrides (opts & Opts.turnLens . Opts.serversSourceLens .~ dnsOpts) $
      responseJsonError
        =<< ( retryWhileN 10 (\r -> statusCode r /= 200) (getTurnConfiguration "v2" uid b)
                <!! const 200 === statusCode
            )
  assertConfiguration
    config
    ( toTurnURI SchemeTurn "turn-udp-5.integration-tests.zinfra.io" 3479 (Just TransportUDP)
        :| [ toTurnURI SchemeTurn "turn-udp-10.integration-tests.zinfra.io" 3478 (Just TransportUDP),
             toTurnURI SchemeTurn "turn-tcp-5.integration-tests.zinfra.io" 3479 (Just TransportTCP),
             toTurnURI SchemeTurn "turn-tcp-10.integration-tests.zinfra.io" 3478 (Just TransportTCP),
             toTurnURI SchemeTurns "turn-tls-5.integration-tests.zinfra.io" 5350 (Just TransportTCP),
             toTurnURI SchemeTurns "turn-tls-10.integration-tests.zinfra.io" 5349 (Just TransportTCP)
           ]
    )

assertConfiguration :: (HasCallStack) => RTCConfiguration -> NonEmpty TurnURI -> Http ()
assertConfiguration cfg expected = do
  let actual = concatMap (toList . view iceURLs) $ toList $ cfg ^. rtcConfIceServers
  liftIO $ assertEqual "Expected adverstised TURN servers to match actual ones" (sort $ toList expected) (sort actual)

getTurnConfigurationV1 :: UserId -> Brig -> Http RTCConfiguration
getTurnConfigurationV1 = getAndValidateTurnConfiguration ""

getTurnConfigurationV2 :: (HasCallStack) => UserId -> Brig -> ((MonadHttp m, MonadIO m, MonadCatch m) => m RTCConfiguration)
getTurnConfigurationV2 = getAndValidateTurnConfiguration "v2"

getTurnConfiguration :: ByteString -> UserId -> Brig -> ((MonadHttp m) => m (Response (Maybe LB.ByteString)))
getTurnConfiguration suffix u b =
  get
    ( b
        . paths ["/calls/config", suffix]
        . zUser u
        . zConn "conn"
    )

getAndValidateTurnConfiguration :: (HasCallStack) => ByteString -> UserId -> Brig -> ((MonadIO m, MonadHttp m, MonadCatch m) => m RTCConfiguration)
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

getAndValidateTurnConfigurationLimit :: (HasCallStack) => Int -> UserId -> Brig -> Http RTCConfiguration
getAndValidateTurnConfigurationLimit limit u b =
  responseJsonError =<< (getTurnConfigurationV2Limit limit u b <!! const 200 === statusCode)

toTurnURILegacy :: ByteString -> Port -> TurnURI
toTurnURILegacy h p = toTurnURI SchemeTurn h p Nothing

toTurnURI :: Scheme -> ByteString -> Port -> Maybe Transport -> TurnURI
toTurnURI s h = turnURI s ip
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
