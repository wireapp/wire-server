{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Test.Brig where

import API.Brig as BrigP
import qualified API.BrigInternal as BrigI
import API.Common
import API.GalleyInternal (setTeamFeatureStatus)
import API.Spar
import Data.Aeson.Types hiding ((.=))
import Data.List.Split
import Data.String.Conversions
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import GHC.Stack
import SAML2.WebSSO.Test.Util (SampleIdP (..), makeSampleIdPMetadata)
import SetupHelpers
import System.IO.Extra
import Testlib.Assertions
import Testlib.Prelude
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
        bindResponse (BrigP.searchTeam owner email) $ \resp -> do
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

registerTestIdPWithMeta :: (HasCallStack, MakesValue owner) => owner -> App Response
registerTestIdPWithMeta owner = do
  SampleIdP idpmeta _ _ _ <- makeSampleIdPMetadata
  createIdp owner idpmeta
