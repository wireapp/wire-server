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

type TurnUpdater = String -> IO ()

tests :: Manager -> Brig -> FilePath -> FilePath -> IO TestTree
tests m b turn turnV2 = do
    return $ testGroup "turn"
        [ test m "basic /calls/config - 200"            $ resetTurn >> testCallsConfig b
        -- FIXME: requires tests to run on same host as brig
        , test m "multiple servers /calls/config - 200" $ resetTurn >> testCallsConfigMultiple b (setTurn turn)
        , test m "multiple servers /calls/config/v2 - 200" $ resetTurn >> testCallsConfigMultipleV2 b (setTurn turnV2)
        ]
  where
    resetTurn = liftIO $ setTurn turn "turn:127.0.0.1:3478" >> setTurn turnV2 "turn:localhost:3478"

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
    let _changes  = "turn:127.0.0.2:3478\nturn:127.0.0.3:3478"
    let _expected = List1.list1 (toTurnURILegacy "127.0.0.2" 3478)
                                [toTurnURILegacy "127.0.0.3" 3478]
    modifyAndAssert b uid getTurnConfigurationV1 turnUpdater _changes _expected

    -- Change server list yet again, try adding transport and ensure that it gets dropped
    let _changes  = "turn:127.0.0.2:3478?transport=udp\nturn:127.0.0.3:3478?transport=udp"
    modifyAndAssert b uid getTurnConfigurationV1 turnUpdater _changes _expected

    -- Revert the config file back to the original
    let _expected = List1.singleton (toTurnURILegacy "127.0.0.1" 3478)
    modifyAndAssert b uid getTurnConfigurationV1 turnUpdater "turn:127.0.0.1:3478" _expected

modifyAndAssert :: Brig
               -> UserId
               -> (UserId -> Brig -> Http RTCConfiguration)
               -> (String -> IO ())
               -> String
               -> List1 TurnURI
               -> Http ()
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
    let _changes  = "turn:localhost:3478\nturn:localhost:3479"
    let _expected = List1.list1 (toTurnURI SchemeTurn "localhost" 3478 Nothing)
                                [toTurnURI SchemeTurn "localhost" 3479 Nothing]
    modifyAndAssert b uid getTurnConfigurationV2 turnUpdaterV2 _changes _expected

    -- Change server list yet again, change the transport and schema
    let _changes  = "turn:localhost:3478?transport=tcp\nturns:localhost:3479?transport=tcp"
    let _expected = List1.list1 (toTurnURI SchemeTurn  "localhost" 3478 $ Just TransportTCP)
                                [toTurnURI SchemeTurns "localhost" 3479 $ Just TransportTCP]
    modifyAndAssert b uid getTurnConfigurationV2 turnUpdaterV2 _changes _expected

    -- Revert the config file back to the original
    let _expected = List1.singleton (toTurnURI SchemeTurn "localhost" 3478 Nothing)
    modifyAndAssert b uid getTurnConfigurationV2 turnUpdaterV2 "turn:localhost:3478" _expected

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
