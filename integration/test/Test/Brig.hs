{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Brig where

import API.Brig as BrigP
import qualified API.BrigInternal as BrigI
import API.Common
import API.GalleyInternal (setTeamFeatureStatus)
import API.Spar
import Control.Monad.Reader (asks)
import Control.Retry
import Data.Aeson.Types hiding ((.=))
import qualified Data.ByteString.Base64 as B64
import Data.List.Split
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import OpenSSL (withOpenSSL)
import OpenSSL.EVP.Digest (getDigestByName, hmacBS)
import System.Directory (copyFile)
import System.FilePath ((</>))
import Data.String.Conversions
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import GHC.Stack
import SetupHelpers
import System.IO.Extra
import Testlib.Assertions
import Testlib.Prelude
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (finally)
import UnliftIO.Temporary

testCrudFederationRemotes :: (HasCallStack) => App ()
testCrudFederationRemotes = do
  otherDomain <- asString OtherDomain
  withModifiedBackend def $ \ownDomain -> do
    let parseFedConns :: (HasCallStack) => Response -> App [Value]
        parseFedConns resp =
          -- Pick out the list of federation domain configs
          getJSON 200 resp %. "remotes"
            & asList
              -- Enforce that the values are objects and not something else
              >>= traverse (fmap Object . asObject)

        addTest :: (MakesValue fedConn, Ord fedConn2, ToJSON fedConn2, MakesValue fedConn2, HasCallStack) => fedConn -> [fedConn2] -> App ()
        addTest fedConn want = do
          bindResponse (BrigI.createFedConn ownDomain fedConn) $ \res -> do
            addFailureContext ("res = " <> show res) $ res.status `shouldMatchInt` 200
            res2 <- parseFedConns =<< BrigI.readFedConns ownDomain
            sort res2 `shouldMatch` sort want

        updateTest :: (MakesValue fedConn, Ord fedConn2, ToJSON fedConn2, MakesValue fedConn2, HasCallStack) => String -> fedConn -> [fedConn2] -> App ()
        updateTest domain fedConn want = do
          bindResponse (BrigI.updateFedConn ownDomain domain fedConn) $ \res -> do
            addFailureContext ("res = " <> show res) $ res.status `shouldMatchInt` 200
            res2 <- parseFedConns =<< BrigI.readFedConns ownDomain
            sort res2 `shouldMatch` sort want

    dom1 :: String <- (<> ".example.com") . UUID.toString <$> liftIO UUID.nextRandom

    let remote1, remote1' :: BrigI.FedConn
        remote1 = BrigI.FedConn dom1 "no_search" Nothing
        remote1' = remote1 {BrigI.searchStrategy = "full_search", BrigI.restriction = Just []}

        cfgRemotesExpect :: BrigI.FedConn
        cfgRemotesExpect = BrigI.FedConn (cs otherDomain) "full_search" Nothing

    cfgRemotes <- parseFedConns =<< BrigI.readFedConns ownDomain
    cfgRemotes `shouldMatch` ([] @Value)
    -- entries present in the config file can be idempotently added if identical, but cannot be
    -- updated.
    addTest cfgRemotesExpect [cfgRemotesExpect]
    -- create
    addTest remote1 [cfgRemotesExpect, remote1]
    addTest remote1 [cfgRemotesExpect, remote1] -- idempotency
    -- update
    updateTest (BrigI.domain remote1) remote1' [cfgRemotesExpect, remote1']

testCrudOAuthClient :: (HasCallStack) => App ()
testCrudOAuthClient = do
  user <- randomUser OwnDomain def
  let appName = "foobar"
  let url = "https://example.com/callback.html"
  clientId <- bindResponse (BrigI.registerOAuthClient user appName url) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "client_id"
  bindResponse (BrigI.getOAuthClient user clientId) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "application_name" `shouldMatch` appName
    resp.json %. "redirect_url" `shouldMatch` url
  let newName = "barfoo"
  let newUrl = "https://example.com/callback2.html"
  bindResponse (BrigI.updateOAuthClient user clientId newName newUrl) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "application_name" `shouldMatch` newName
    resp.json %. "redirect_url" `shouldMatch` newUrl
  bindResponse (BrigI.deleteOAuthClient user clientId) $ \resp -> do
    resp.status `shouldMatchInt` 200
  bindResponse (BrigI.getOAuthClient user clientId) $ \resp -> do
    resp.status `shouldMatchInt` 404

testCrudFederationRemoteTeams :: (HasCallStack) => App ()
testCrudFederationRemoteTeams = do
  (_, tid, _) <- createTeam OwnDomain 1
  (_, tid2, _) <- createTeam OwnDomain 1
  rd <- (\n -> n <> ".wire.com") <$> randomName
  bindResponse (BrigI.addFederationRemoteTeam' OwnDomain rd tid) $ \resp -> do
    resp.status `shouldMatchInt` 533
  void $ BrigI.createFedConn OwnDomain $ BrigI.FedConn rd "full_search" Nothing
  bindResponse (BrigI.addFederationRemoteTeam' OwnDomain rd tid) $ \resp -> do
    resp.status `shouldMatchInt` 533
  void $ BrigI.updateFedConn OwnDomain rd $ BrigI.FedConn rd "full_search" (Just [])
  bindResponse (BrigI.getFederationRemoteTeams OwnDomain rd) $ \resp -> do
    resp.status `shouldMatchInt` 200
    checkAbsence resp [tid, tid2]
  BrigI.addFederationRemoteTeam OwnDomain rd tid
  bindResponse (BrigI.getFederationRemoteTeams OwnDomain rd) $ \resp -> do
    resp.status `shouldMatchInt` 200
    checkPresence resp [tid]
    checkAbsence resp [tid2]
  BrigI.addFederationRemoteTeam OwnDomain rd tid2
  bindResponse (BrigI.getFederationRemoteTeams OwnDomain rd) $ \resp -> do
    resp.status `shouldMatchInt` 200
    checkPresence resp [tid, tid2]
  BrigI.deleteFederationRemoteTeam OwnDomain rd tid
  bindResponse (BrigI.getFederationRemoteTeams OwnDomain rd) $ \resp -> do
    resp.status `shouldMatchInt` 200
    checkPresence resp [tid2]
    checkAbsence resp [tid]
  BrigI.deleteFederationRemoteTeam OwnDomain rd tid2
  bindResponse (BrigI.getFederationRemoteTeams OwnDomain rd) $ \resp -> do
    resp.status `shouldMatchInt` 200
    checkAbsence resp [tid, tid2]
  where
    checkAbsence :: Response -> [String] -> App ()
    checkAbsence resp tids = do
      l <- resp.json & asList
      remoteTeams <- forM l (\e -> e %. "team_id" & asString)
      when (any (\t -> t `elem` remoteTeams) tids) $ assertFailure "Expected response to not contain any of the teams"

    checkPresence :: Response -> [String] -> App ()
    checkPresence resp tids = do
      l <- resp.json & asList
      remoteTeams <- forM l (\e -> e %. "team_id" & asString)
      when (any (\t -> t `notElem` remoteTeams) tids) $ assertFailure "Expected response to contain all of the teams"

testSFTCredentials :: (HasCallStack) => App ()
testSFTCredentials = do
  let ttl = (60 :: Int)
  withSystemTempFile "sft-secret" $ \secretFile secretHandle -> do
    liftIO $ do
      hPutStr secretHandle "xMtZyTpu=Leb?YKCoq#BXQR:gG^UrE83dNWzFJ2VcD"
      hClose secretHandle
    withModifiedBackend
      ( def
          { brigCfg =
              ( setField "sft.sftBaseDomain" "integration-tests.zinfra.io"
                  . setField "sft.sftToken.ttl" ttl
                  . setField "sft.sftToken.secret" secretFile
                  . setField "sft.sftDiscoveryIntervalSeconds" (1 :: Int)
                  . setField "optSettings.setSftListAllServers" "enabled"
              )
          }
      )
      $ \domain -> do
        user <- randomUser domain def

        sftServersAll <-
          retrying
            (limitRetriesByCumulativeDelay 2_000_000 $ fullJitterBackoff 50_000)
            (\_ xs -> let shouldRetry = null xs in pure shouldRetry)
            $ \_ -> do
              resp <- getCallsConfigV2 user
              resp.json %. "sft_servers_all" & asList

        when (null sftServersAll) $ assertFailure "sft_servers_all empty"
        for_ sftServersAll $ \s -> do
          cred <- s %. "credential" & asString
          when (null cred) $ assertFailure "credential missing"
          usr <- s %. "username" & asString
          let parts = splitOn "." usr
          when (length parts /= 5) $ assertFailure "username should have 5 parts"
          when (take 2 (head parts) /= "d=") $ assertFailure "missing expiry time identifier"
          when (take 2 (parts !! 1) /= "v=") $ assertFailure "missing version identifier"
          when (take 2 (parts !! 2) /= "k=") $ assertFailure "missing key ID identifier"
          when (take 2 (parts !! 3) /= "s=") $ assertFailure "missing federation identifier"
          when (take 2 (parts !! 4) /= "r=") $ assertFailure "missing random data identifier"
          for_ parts $ \part -> when (length part < 3) $ assertFailure ("value missing for " <> part)

testSFTNoCredentials :: (HasCallStack) => App ()
testSFTNoCredentials = withModifiedBackend
  ( def
      { brigCfg =
          ( setField "sft.sftBaseDomain" "integration-tests.zinfra.io"
              . setField "optSettings.setSftListAllServers" "enabled"
          )
      }
  )
  $ \domain -> do
    user <- randomUser domain def
    bindResponse (getCallsConfigV2 user) \resp -> do
      sftServersAll <- resp.json %. "sft_servers_all" & asList
      when (null sftServersAll) $ assertFailure "sft_servers_all missing"
      for_ sftServersAll $ \s -> do
        credM <- lookupField s "credential"
        when (isJust credM) $ assertFailure "should not generate credential"
        usrM <- lookupField s "username"
        when (isJust usrM) $ assertFailure "should not generate username"

testSFTFederation :: (HasCallStack) => App ()
testSFTFederation = do
  withModifiedBackend
    ( def
        { brigCfg =
            ( setField "sft.sftBaseDomain" "integration-tests.zinfra.io"
                . removeField "multiSFT"
            )
        }
    )
    $ \domain -> do
      user <- randomUser domain def
      bindResponse (getCallsConfigV2 user) \resp -> do
        isFederatingM <- lookupField resp.json "is_federating"
        when (isJust isFederatingM) $ assertFailure "is_federating should not be present"
  withModifiedBackend
    ( def
        { brigCfg =
            ( setField "sft.sftBaseDomain" "integration-tests.zinfra.io"
                . setField "multiSFT" True
            )
        }
    )
    $ \domain -> do
      user <- randomUser domain def
      bindResponse (getCallsConfigV2 user) \resp -> do
        isFederating <-
          maybe (assertFailure "is_federating missing") asBool
            =<< lookupField resp.json "is_federating"
        unless isFederating $ assertFailure "is_federating should be true"
  withModifiedBackend
    ( def
        { brigCfg =
            ( setField "sft.sftBaseDomain" "integration-tests.zinfra.io"
                . setField "multiSFT" False
            )
        }
    )
    $ \domain -> do
      user <- randomUser domain def
      bindResponse (getCallsConfigV2 user) \resp -> do
        isFederating <-
          maybe (assertFailure "is_federating missing") asBool
            =<< lookupField resp.json "is_federating"
        when isFederating $ assertFailure "is_federating should be false"

-- | Backs up @cfgDest@, runs the action with an updater that replaces the file
-- content (via copy so brig's filewatch notices), and restores the backup. The
-- 1s sleep after each write must exceed brig's 0.5s watcher interval.
withModifiedTurnFile :: FilePath -> ((String -> App ()) -> App ()) -> App ()
withModifiedTurnFile cfgDest action =
  withSystemTempDirectory "wire.temp" $ \tmpDir -> do
    let backup = tmpDir </> "turn.bak"
    liftIO $ copyFile cfgDest backup
    action (\s -> setTurn tmpDir cfgDest s) `finally` liftIO (copyFile backup cfgDest)

-- | This essentially writes 'newConf' to @cfgDest@, but in a portable way that
-- makes sure brig's filewatch notices that the file has changed.
setTurn :: FilePath -> FilePath -> String -> App ()
setTurn tmpDir cfgDest newConf = do
  let tmpFile = tmpDir </> "turn.txt"
  liftIO $ writeFile tmpFile newConf
  liftIO $ copyFile tmpFile cfgDest
  threadDelay 1000000

-- | Asserts coturn native long-term credentials on a @/calls/config/v3@
-- response: @username = "<unix-expiry>:<uid>"@ with future expiry and non-empty
-- [a-z0-9] uid, and @credential = base64(HMAC-SHA1(secret, username))@.
assertCoturnCredentials :: (HasCallStack) => FilePath -> Value -> App ()
assertCoturnCredentials secretPath cfg = do
  raw <- liftIO (readFile secretPath)
  let secret = cs (Text.strip (cs raw :: Text.Text)) :: ByteString
  now <- liftIO getPOSIXTime
  mSha1 <- liftIO . withOpenSSL $ getDigestByName "SHA1"
  sha1 <- maybe (assertFailure "SHA1 digest not available") pure mSha1
  iceServers <- cfg %. "ice_servers" & asList
  for_ iceServers $ \ice -> do
    urls <- ice %. "urls" & asList >>= traverse asString
    when (null urls) $ assertFailure "ice server urls missing"
    uname <- ice %. "username" & asString
    let (expiryStr, ':' : uid') = break (== ':') uname
    assertBool "coturn username expiry should be numeric" (all isDigit expiryStr)
    assertBool "coturn username expiry should be in the future" (realToFrac (read expiryStr :: Integer) > (now :: POSIXTime))
    assertBool "coturn username uid should be non-empty [a-z0-9]" (all (\c -> isAsciiLower c || isDigit c) uid' && not (null uid'))
    cred <- ice %. "credential" & asString
    let expected = cs (B64.encode (hmacBS sha1 secret (cs uname :: ByteString))) :: String
    cred `shouldMatch` expected

-- | multiple servers using files /calls/config/v3 - 200
--
-- Rewrites the turn v2 servers file watched by backendA's brig (config keys
-- 'turn.serversV2' and 'turn.coturnSecret') and verifies that the v3 endpoint
-- advertises exactly the file's servers with valid coturn native long-term
-- credentials.
testCallsConfigV3 :: (HasCallStack) => App ()
testCallsConfigV3 = do
  mServicesCwd <- asks (.servicesCwdBase)
  brigCwd <- case mServicesCwd of
    Nothing -> failApp "turn file tests require locally running services"
    Just scwd -> pure (scwd </> "brig")
  cfg <- readServiceConfig Brig
  serversV2Rel <- cfg %. "turn" %. "serversV2" & asString
  secretRel <- cfg %. "turn" %. "coturnSecret" & asString
  let serversV2 = brigCwd </> serversV2Rel
      secretAbs = brigCwd </> secretRel
  user <- randomUser OwnDomain def
  withModifiedTurnFile serversV2 $ \updater -> do
    -- Ensure we have a clean config
    updater "turn:localhost:3478"
    assertV3Phase user secretAbs ["turn:localhost:3478"]
    -- Change server list
    updater "turn:localhost:3478\nturn:localhost:3479"
    assertV3Phase user secretAbs ["turn:localhost:3478", "turn:localhost:3479"]
    -- Revert the config file back to the original
    updater "turn:localhost:3478"
    assertV3Phase user secretAbs ["turn:localhost:3478"]

assertV3Phase :: (HasCallStack) => MakesValue user => user -> FilePath -> [String] -> App ()
assertV3Phase user secretAbs expected = do
  bindResponse (getCallsConfigV3 user) \resp -> do
    resp.status `shouldMatchInt` 200
    iceServers <- resp.json %. "ice_servers" & asList
    urls <- sort . concat <$> forM iceServers (\e -> e %. "urls" & asList >>= traverse asString)
    urls `shouldMatch` sort expected
    body <- maybe (assertFailure "no response body") pure resp.json
    assertCoturnCredentials secretAbs body

testDeleteEmail :: (HasCallStack) => App ()
testDeleteEmail = do
  (owner, tid, [usr]) <- createTeam OwnDomain 2
  putSelf usr (PutSelf Nothing Nothing (Just "Alice") Nothing) >>= assertSuccess
  email <- getSelf usr >>= getJSON 200 >>= (%. "email") >>= asString

  let associateUsrWithSSO :: (HasCallStack) => App ()
      associateUsrWithSSO = do
        void $ setTeamFeatureStatus owner tid "sso" "enabled"
        registerTestIdPWithMeta owner >>= assertSuccess
        tok <- createScimTokenV6 owner def >>= getJSON 200 >>= (%. "token") >>= asString
        void $ findUsersByExternalId owner tok email

      searchShouldBe :: (HasCallStack) => String -> App ()
      searchShouldBe expected = do
        BrigI.refreshIndex OwnDomain
        bindResponse (BrigP.searchTeamWithSearchTerm owner email) $ \resp -> do
          resp.status `shouldMatchInt` 200
          numDocs <- length <$> (resp.json %. "documents" >>= asList)
          case expected of
            "empty" -> numDocs `shouldMatchInt` 0
            "non-empty" -> numDocs `shouldMatchInt` 1

  deleteSelfEmail usr >>= assertStatus 403
  searchShouldBe "non-empty"
  associateUsrWithSSO
  deleteSelfEmail usr >>= assertSuccess
  searchShouldBe "empty"
