{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module API.TURN where

import Bilge
import Bilge.Assert
import Brig.Types hiding (Handle)
import Control.Concurrent (threadDelay)
import Control.Lens ((^.))
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.ByteString.Conversion
import Data.Id
import Data.Foldable
import Data.List ((\\))
import Data.List1 (List1)
import Data.Maybe (fromMaybe)
import Data.Misc (Port)
import GHC.Stack (HasCallStack)
import Network.HTTP.Client (Manager)
import System.IO.Temp (writeTempFile)
import System.FilePath.Posix (FilePath)
import System.Directory (copyFile, removeFile, getTemporaryDirectory)
import Test.Tasty
import Test.Tasty.HUnit
import Util

import qualified Data.ByteString.Lazy as LB
import qualified Data.List1 as List1
import qualified Network.Wai.Utilities.Error as Error

type TurnUpdater = String -> IO ()

tests :: Manager -> Brig -> FilePath -> IO TestTree
tests m b t = do
    return $ testGroup "turn"
        [ test m "basic /calls/config - 200"            $ resetTurn >> testCallsConfig b
        -- FIXME: requires tests to run on same host as brig
        , test m "multiple servers /calls/config - 200" $ resetTurn >> testCallsConfigMultiple b (setTurn t)
        ]
  where
    resetTurn = liftIO $ setTurn t "turn:127.0.0.1:3478"

testCallsConfig :: Brig -> Http ()
testCallsConfig b = do
    uid <- userId <$> randomUser b
    cfg <- getTurnConfigurationV1 uid b
    let _expectedV1 = List1.singleton (toTurnURILegacy "127.0.0.1" 3478)
    assertConfiguration cfg _expectedV1

testCallsConfigMultiple :: Brig -> TurnUpdater -> Http ()
testCallsConfigMultiple b st = do
    uid  <- userId <$> randomUser b
    -- Ensure we have a clean config
    _cfg <- getTurnConfigurationV1 uid b
    let _expected = List1.singleton (toTurnURILegacy "127.0.0.1" 3478)
    assertConfiguration _cfg _expected

    -- Change server list
    let _changes    = "turn:127.0.0.2:3478\nturn:127.0.0.3:3478"
    let _expectedV1 = List1.list1 (toTurnURILegacy "127.0.0.2" 3478)
                                  [toTurnURILegacy "127.0.0.3" 3478]
    liftIO $ st _changes
    _cfg1 <- getTurnConfigurationV1 uid b
    assertConfiguration _cfg1 _expectedV1
    -- With this configuration, the V2 endpoint will not return any TURN server
    getTurnConfiguration "v2" uid b !!! do
        const 500                            === statusCode
        const (Just "incorrect-turn-config") === fmap Error.label . decodeBody

    -- Change server list, more transport options. Only the legacy endpoint, only `turn`
    -- and no transport (in practice, that's udp) are to be returned; i.e., `turn` and `udp`
    -- endpoints are returned but the `transport` suffix is removed
    let _changes    = "turn:127.0.0.2:3479?transport=udp\nturn:localhost:3480?transport=tcp"
    let _expectedV1 = List1.singleton (toTurnURILegacy "127.0.0.2" 3479)
    let _expectedV2 = List1.singleton (toTurnURI SchemeTurn "localhost" 3480 $ Just TransportTCP)
    modifyAndAssert uid _changes _expectedV1 _expectedV2

    -- Change server list yet again, different schemas too - bad config for V1(!) so test it separately
    let _changes  = "turns:localhost:3489?transport=tcp\nturns:localhost:3490?transport=tcp"
    let _expectedV2 = List1.list1 (toTurnURI SchemeTurns "localhost" 3489 $ Just TransportTCP)
                                  [toTurnURI SchemeTurns "localhost" 3490 $ Just TransportTCP]
    liftIO $ st _changes
    _cfg2 <- getTurnConfigurationV2 uid b
    assertConfiguration _cfg2 _expectedV2
    -- With this configuration, the legacy endpoint will not return any TURN server
    getTurnConfiguration "" uid b !!! do
        const 500                            === statusCode
        const (Just "incorrect-turn-config") === fmap Error.label . decodeBody

    -- Revert the config file back to the original
    let _changes = "turn:127.0.0.1:3478"
    liftIO $ st _changes
    _cfg <- getTurnConfigurationV1 uid b
    let _expected = List1.singleton (toTurnURILegacy "127.0.0.1" 3478)
    assertConfiguration _cfg _expected
  where
    modifyAndAssert uid newServers expectedV1 expectedV2 = do
        liftIO $ st newServers
        cfg1 <- getTurnConfigurationV1 uid b
        assertConfiguration cfg1 expectedV1
        cfg2 <- getTurnConfigurationV2 uid b
        assertConfiguration cfg2 expectedV2

assertConfiguration :: HasCallStack => RTCConfiguration -> List1 TurnURI -> Http ()
assertConfiguration cfg turns =
    checkIceServers (toList $ cfg^.rtcConfIceServers) (toList turns)
  where
    -- Each turn server should _only_ be in 1 configuration
    checkIceServers :: [RTCIceServer] -> [TurnURI] -> Http ()
    checkIceServers []     [] = return ()
    checkIceServers []     ts = error ("TURN servers not advertised: " ++ show ts)
    checkIceServers (x:xs) ts = do
        remaining <- checkIceServer x ts
        checkIceServers xs remaining

    checkIceServer :: RTCIceServer -> [TurnURI] -> Http [TurnURI]
    checkIceServer srv uris = do
        let iceURIs = toList (srv^.iceURLs)
        liftIO $ assertBool (diff iceURIs uris) (all (`elem` uris) iceURIs)
        return (uris \\ iceURIs)
      where
        diff advertised expected = "Some advertised URIs not expected, advertised: "
            ++ show advertised ++ " expected: " ++ show expected

getTurnConfigurationV1 :: UserId -> Brig -> Http RTCConfiguration
getTurnConfigurationV1 = getAndValidateTurnConfiguration ""

getTurnConfigurationV2 :: UserId -> Brig -> Http RTCConfiguration
getTurnConfigurationV2 = getAndValidateTurnConfiguration "v2"

getTurnConfiguration :: ByteString -> UserId -> Brig -> Http (Response (Maybe LB.ByteString))
getTurnConfiguration suffix u b = get ( b
                                . paths ["/calls/config", suffix]
                                . zUser u
                                . zConn "conn"
                                )

getAndValidateTurnConfiguration :: HasCallStack => ByteString -> UserId -> Brig -> Http RTCConfiguration
getAndValidateTurnConfiguration suffix u b = do
    r <- getTurnConfiguration suffix u b <!! const 200 === statusCode
    return $ fromMaybe (error "getTurnConfiguration: failed to parse response") (decodeBody r)

toTurnURILegacy :: ByteString -> Port -> TurnURI
toTurnURILegacy h p = toTurnURI SchemeTurn h p Nothing

toTurnURI :: Scheme -> ByteString -> Port -> Maybe Transport -> TurnURI
toTurnURI s h p t = turnURI s ip p t
  where
    ip = fromMaybe (error "Failed to parse host address")
       $ fromByteString h

setTurn :: FilePath -> String -> IO ()
setTurn cfgDest newConf = do
    tmpDir <- getTemporaryDirectory
    tmpPathFile <- writeTempFile tmpDir "-turn.tmp" newConf
    copyFile tmpPathFile cfgDest
    -- TODO: This must be higher than the value specified
    -- in the watcher in Brig.App (currently, 0.5 seconds)
    threadDelay 1000000
    -- Note that this may leave temporary files behind in
    -- case of some exceptions
    removeFile tmpPathFile
