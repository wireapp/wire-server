{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-} 

module API.TURN where

import Bilge
import Bilge.Assert
import Brig.Types hiding (Handle)
import Control.Concurrent (threadDelay)
import Control.Lens ((^.), (#))
import Control.Monad.IO.Class
import Data.Id
import Data.Foldable
import Data.List ((\\))
import Data.List1 (List1)
import Data.Maybe (fromMaybe)
import Data.Misc (Port)
import Network.HTTP.Client (Manager)
import Safe (readMay)
import System.IO.Temp (writeTempFile)
import System.FilePath.Posix (FilePath)
import System.Directory (copyFile, removeFile, getTemporaryDirectory)
import Test.Tasty
import Test.Tasty.HUnit
import Util

import qualified Data.List1   as List1

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
    cfg <- getTurnConfiguration uid b
    let expected = List1.singleton (toTurnURILegacy "127.0.0.1" 3478)
    assertConfiguration cfg expected

testCallsConfigMultiple :: Brig -> TurnUpdater -> Http ()
testCallsConfigMultiple b st = do
    uid  <- userId <$> randomUser b
    -- Ensure we have a clean config
    _cfg <- getTurnConfiguration uid b
    let _expected = List1.singleton (toTurnURILegacy "127.0.0.1" 3478)
    assertConfiguration _cfg _expected

    -- Change server list
    let _changes  = "turn:127.0.0.2:3478\nturn:127.0.0.3:3478"
    let _expected = List1.list1 (toTurnURILegacy "127.0.0.2" 3478)
                                [toTurnURILegacy "127.0.0.3" 3478]
    modifyAndAssert uid _changes _expected

    -- Change server list, more transport options
    let _changes  = "turn:127.0.0.2:3479?transport=udp\nturn:127.0.0.3:3480?transport=tcp"
    let _expected = List1.list1 (toTurnURI SchemeTurn "127.0.0.2" 3479 $ Just TransportUDP)
                                [toTurnURI SchemeTurn "127.0.0.3" 3480 $ Just TransportTCP]
    modifyAndAssert uid _changes _expected

    -- Change server list yet again, different schemas too
    let _changes  = "turns:127.0.0.4:3489?transport=tcp\nturns:127.0.0.5:3490?transport=tcp"
    let _expected = List1.list1 (toTurnURI SchemeTurns "127.0.0.4" 3489 $ Just TransportTCP)
                                [toTurnURI SchemeTurns "127.0.0.5" 3490 $ Just TransportTCP]
    modifyAndAssert uid _changes _expected

    -- Revert the config file back to the original
    let _changes  = "turn:127.0.0.1:3478"
    let _expected = List1.singleton (toTurnURILegacy "127.0.0.1" 3478)
    modifyAndAssert uid _changes _expected
  where
    modifyAndAssert uid newServers expected = do
        liftIO $ st newServers
        cfg <- getTurnConfiguration uid b
        assertConfiguration cfg expected

assertConfiguration :: RTCConfiguration -> List1 TurnURI -> Http ()
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

getTurnConfiguration :: UserId -> Brig -> Http RTCConfiguration
getTurnConfiguration u b = do
    r <- get ( b 
             . path "/calls/config"
             . zUser u
             . zConn "conn") <!! const 200 === statusCode
    return $ fromMaybe (error "getTurnConfiguration: failed to parse response") (decodeBody r)

toTurnURILegacy :: String -> Port -> TurnURI
toTurnURILegacy h p = toTurnURI SchemeTurn h p Nothing

toTurnURI :: Scheme -> String -> Port -> Maybe Transport -> TurnURI
toTurnURI s h p t = turnURI s (_TurnHost # ip) p t
  where
    ip = fromMaybe (error "Failed to parse ip address")
       $ readMay h

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
