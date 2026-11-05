{-# OPTIONS -Wno-ambiguous-fields #-}
-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2024 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Teams where

import API.Brig
import qualified API.BrigInternal as I
import API.Common
import API.Galley (deleteTeamMember, getTeam, getTeamMembers, getTeamMembersCsv, getTeamNotifications, putResendTeamMemberVerificationEmail, setTeamFeatureConfig)
import API.GalleyInternal (selectTeamMembers)
import qualified API.GalleyInternal as I
import API.Gundeck
import qualified API.Nginz as Nginz
import API.Spar (createScimToken, createScimUser, idp)
import Control.Monad.Codensity (Codensity (runCodensity))
import Control.Monad.Extra (findM)
import Control.Monad.Reader (asks)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time.Clock
import Data.Time.Format
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import MLS.Util (InitMLSClient (..), createMLSClient, uploadNewKeyPackage)
import Notifications
import SetupHelpers
import Testlib.JSON
import Testlib.Prelude
import Testlib.ResourcePool (acquireResources)
import Testlib.VersionedFed (FedDomain)

testInvitePersonalUserToTeam :: (HasCallStack) => App ()
testInvitePersonalUserToTeam = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[testBackend] -> do
    let domain = testBackend.berDomain
    (owner, tid, tm) <- runCodensity (startDynamicBackend testBackend def) $ \_ -> do
      (owner, tid, tm : _) <- createTeam domain 2
      pure (owner, tid, tm)

    runCodensity
      ( startDynamicBackend
          testBackend
          (def {galleyCfg = setField "settings.exposeInvitationURLsTeamAllowlist" [tid]})
      )
      $ \_ -> do
        bindResponse (listInvitations owner tid) $ \resp -> do
          resp.status `shouldMatchInt` 200
          resp.json %. "invitations" `shouldMatch` ([] :: [()])

        ownerId <- owner %. "id" & asString
        I.setTeamFeatureStatus domain tid "exposeInvitationURLsToTeamAdmin" "enabled" >>= assertSuccess
        user <- I.createUser domain def >>= getJSON 201
        uid <- user %. "id" >>= asString
        email <- user %. "email" >>= asString

        inv <- postInvitation owner (PostInvitation (Just email) Nothing) >>= getJSON 201
        checkListInvitations owner tid email
        code <- I.getInvitationCode owner inv >>= getJSON 200 >>= (%. "code") & asString
        inv %. "url" & asString >>= assertUrlContainsCode code
        bindResponse (getInvitationByCode user code) $ \resp -> do
          resp.status `shouldMatchInt` 200
          ownersEmail <- owner %. "email" & asString
          resp.json %. "created_by_email" `shouldMatch` ownersEmail
        acceptTeamInvitation user code Nothing >>= assertStatus 400
        acceptTeamInvitation user code (Just "wrong-password") >>= assertStatus 403

        withWebSockets [owner, user, tm] $ \wss@[wsOwner, _, _] -> do
          acceptTeamInvitation user code (Just defPassword) >>= assertSuccess

          -- When the team is smaller than fanout limit, all members get this
          -- notification.
          for_ wss $ \ws -> do
            updateNotif <- awaitMatch isUserUpdatedNotif ws
            updateNotif %. "payload.0.user.team" `shouldMatch` tid

          -- Admins get a team.member-join notif on the websocket for
          -- team-settings
          memberJobNotif <- awaitMatch isTeamMemberJoinNotif wsOwner
          memberJobNotif %. "payload.0.team" `shouldMatch` tid
          memberJobNotif %. "payload.0.data.user" `shouldMatch` objId user

        bindResponse (getSelf user) $ \resp -> do
          resp.status `shouldMatchInt` 200
          resp.json %. "team" `shouldMatch` tid

        -- a team member can now find the former personal user in the team
        bindResponse (getTeamMembers tm tid) $ \resp -> do
          resp.status `shouldMatchInt` 200
          members <- resp.json %. "members" >>= asList
          ids <- for members ((%. "user") >=> asString)
          ids `shouldContain` [uid]

        -- the former personal user can now see other team members
        bindResponse (getTeamMembers user tid) $ \resp -> do
          resp.status `shouldMatchInt` 200
          members <- resp.json %. "members" >>= asList
          ids <- for members ((%. "user") >=> asString)
          tmId <- tm %. "id" & asString
          ids `shouldContain` [ownerId]
          ids `shouldContain` [tmId]

        -- the former personal user can now search for the owner
        bindResponse (searchContacts user (owner %. "name") domain) $ \resp -> do
          resp.status `shouldMatchInt` 200
          documents <- resp.json %. "documents" >>= asList
          ids <- for documents ((%. "id") >=> asString)
          ids `shouldContain` [ownerId]

        I.refreshIndex domain
        -- a team member can now search for the former personal user
        bindResponse (searchContacts tm (user %. "name") domain) $ \resp -> do
          resp.status `shouldMatchInt` 200
          document <- resp.json %. "documents" >>= asList >>= assertOne
          document %. "id" `shouldMatch` uid
          document %. "team" `shouldMatch` tid
  where
    checkListInvitations :: Value -> String -> String -> App ()
    checkListInvitations owner tid email = do
      newUserEmail <- randomEmail
      inv <- postInvitation owner (PostInvitation (Just newUserEmail) Nothing) >>= getJSON 201
      code <- I.getInvitationCode owner inv >>= getJSON 200 >>= (%. "code") & asString
      bindResponse (getInvitationByCode owner code) $ \resp -> do
        resp.status `shouldMatchInt` 200
        lookupField resp.json "created_by_email" `shouldMatch` (Nothing :: Maybe Value)
      bindResponse (listInvitations owner tid) $ \resp -> do
        resp.status `shouldMatchInt` 200
        invitations <- resp.json %. "invitations" >>= asList

        -- personal user invitations have a different invitation URL than non-existing user invitations
        newUserInv <- invitations & findM (\i -> (i %. "email" >>= asString) <&> (== newUserEmail))
        newUserInvUrl <- newUserInv %. "url" & asString
        newUserInvUrl `shouldContainString` "/join"

        personalUserInv <- invitations & findM (\i -> (i %. "email" >>= asString) <&> (== email))
        personalUserInvUrl <- personalUserInv %. "url" & asString
        personalUserInvUrl `shouldContainString` "/accept-invitation"

    assertUrlContainsCode :: (HasCallStack) => String -> String -> App ()
    assertUrlContainsCode code url = do
      queryParam <- url & asString <&> getQueryParam "team-code"
      queryParam `shouldMatch` Just (Just code)

testInvitePersonalUserToLargeTeam :: (HasCallStack) => App ()
testInvitePersonalUserToLargeTeam = do
  teamSize <- readServiceConfig Galley %. "settings.maxFanoutSize" & asInt <&> (+ 1)
  (owner, tid, (alice : otherTeamMembers)) <- createTeam OwnDomain teamSize
  -- User to be invited to the team
  knut <- I.createUser OwnDomain def >>= getJSON 201

  -- Non team friends of knut
  dawn <- I.createUser OwnDomain def >>= getJSON 201
  eli <- I.createUser OtherDomain def >>= getJSON 201

  -- knut is also friends with alice, but not any other team members.
  traverse_ (connectTwoUsers knut) [alice, dawn, eli]

  addFailureContext ("tid: " <> tid) $ do
    let uids = [("owner", owner), ("alice", alice), ("knut", knut), ("dawn", dawn), ("eli", eli)]
    addUsersToFailureContext uids $ do
      lastTeamNotif <-
        getTeamNotifications owner Nothing `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          resp.json %. "notifications.-1.id" & asString

      knutEmail <- knut %. "email" >>= asString
      inv <- postInvitation owner (PostInvitation (Just knutEmail) Nothing) >>= getJSON 201
      code <- I.getInvitationCode owner inv >>= getJSON 200 >>= (%. "code") & asString

      withWebSockets [owner, alice, dawn, eli, head otherTeamMembers] $ \[wsOwner, wsAlice, wsDawn, wsEli, wsOther] -> do
        acceptTeamInvitation knut code (Just defPassword) >>= assertSuccess

        for_ [wsAlice, wsDawn] $ \ws -> do
          notif <- awaitMatch isUserUpdatedNotif ws
          nPayload notif %. "user.id" `shouldMatch` (objId knut)
          nPayload notif %. "user.team" `shouldMatch` tid

        -- Admins get a team.member-join notif on the websocket for
        -- team-settings
        memberJobNotif <- awaitMatch isTeamMemberJoinNotif wsOwner
        memberJobNotif %. "payload.0.team" `shouldMatch` tid
        memberJobNotif %. "payload.0.data.user" `shouldMatch` objId knut

        -- Other team members don't get notified on the websocket
        assertNoEvent 1 wsOther

        -- Remote users are not notified at all
        assertNoEvent 1 wsEli

      -- Other team members learn about knut via team notifications
      getTeamNotifications (head otherTeamMembers) (Just lastTeamNotif) `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        -- Ignore the first notif because it is always the notif matching the
        -- lastTeamNotif id.
        resp.json %. "notifications.1.payload.0.type" `shouldMatch` "team.member-join"
        resp.json %. "notifications.1.payload.0.team" `shouldMatch` tid
        resp.json %. "notifications.1.payload.0.data.user" `shouldMatch` objId knut

testInvitePersonalUserToTeamMultipleInvitations :: (HasCallStack) => App ()
testInvitePersonalUserToTeamMultipleInvitations = do
  (owner, tid, _) <- createTeam OwnDomain 0
  (owner2, _, _) <- createTeam OwnDomain 0
  user <- I.createUser OwnDomain def >>= getJSON 201
  email <- user %. "email" >>= asString
  inv <- postInvitation owner (PostInvitation (Just email) Nothing) >>= getJSON 201
  inv2 <- postInvitation owner2 (PostInvitation (Just email) Nothing) >>= getJSON 201
  code <- I.getInvitationCode owner inv >>= getJSON 200 >>= (%. "code") & asString
  acceptTeamInvitation user code (Just defPassword) >>= assertSuccess
  bindResponse (getSelf user) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "team" `shouldMatch` tid
  code2 <- I.getInvitationCode owner2 inv2 >>= getJSON 200 >>= (%. "code") & asString
  bindResponse (acceptTeamInvitation user code2 (Just defPassword)) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "cannot-join-multiple-teams"
  bindResponse (getSelf user) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "team" `shouldMatch` tid
  acceptTeamInvitation user code (Just defPassword) >>= assertStatus 400

testInvitePersonalUserToTeamEmailDomainForAnotherBackend :: (HasCallStack) => App ()
testInvitePersonalUserToTeamEmailDomainForAnotherBackend = forM_ [ExplicitVersion 8, Versioned] \version -> do
  domain <- randomDomain

  (owner, _, _) <- createTeam OwnDomain 0
  let email = "user@" <> domain
  void $ I.createUser OwnDomain def {I.email = Just email} >>= getJSON 201

  I.domainRegistrationPreAuthorize OwnDomain domain >>= assertStatus 204
  setup <- setupOwnershipTokenForBackend OwnDomain domain
  -- [customer admin] post no-registration config
  updateDomainRedirect
    OwnDomain
    version
    domain
    (Just setup.ownershipToken)
    (mkDomainRedirectBackend version "https://example.com" "https://webapp.example.com")
    >>= assertStatus 200

  postInvitation owner (PostInvitation (Just email) Nothing) `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "condition-failed"

testAcceptInvitePersonalUserToTeamEmailDomainForAnotherBackend :: (HasCallStack) => App ()
testAcceptInvitePersonalUserToTeamEmailDomainForAnotherBackend = forM_ [ExplicitVersion 8, Versioned] \version -> do
  domain <- randomDomain

  (owner, _, _) <- createTeam OwnDomain 0
  let email = "user@" <> domain
  user <- I.createUser OwnDomain def {I.email = Just email} >>= getJSON 201
  inv <- postInvitation owner (PostInvitation (Just email) Nothing) >>= getJSON 201
  code <- I.getInvitationCode owner inv >>= getJSON 200 >>= (%. "code") & asString

  I.domainRegistrationPreAuthorize OwnDomain domain >>= assertStatus 204
  setup <- setupOwnershipTokenForBackend OwnDomain domain

  -- [customer admin] post no-registration config
  updateDomainRedirect
    OwnDomain
    version
    domain
    (Just setup.ownershipToken)
    (mkDomainRedirectBackend version "https://example.com" "https://webapp.example.com")
    >>= assertStatus 200

  acceptTeamInvitation user code (Just defPassword) `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "condition-failed"

testInvitePersonalUserToTeamLegacy :: (HasCallStack) => App ()
testInvitePersonalUserToTeamLegacy = withAPIVersion 6 $ do
  (owner, tid, _) <- createTeam OwnDomain 0
  user <- I.createUser OwnDomain def >>= getJSON 201

  -- inviting an existing user should fail
  do
    email <- user %. "email" >>= asString
    bindResponse (postInvitation owner (PostInvitation (Just email) Nothing)) $ \resp -> do
      resp.status `shouldMatchInt` 409
      resp.json %. "label" `shouldMatch` "email-exists"

  -- inviting a new user should succeed
  do
    email <- randomEmail
    bindResponse (postInvitation owner (PostInvitation (Just email) Nothing)) $ \resp -> do
      resp.status `shouldMatchInt` 201
      resp.json %. "email" `shouldMatch` email
      resp.json %. "team" `shouldMatch` tid

testInvitationTypesAreDistinct :: (HasCallStack) => Domain -> App ()
testInvitationTypesAreDistinct domain = do
  -- Test all domains here to make sure that enterprise login being disabled
  -- does not prevent user registration.

  -- We are only testing one direction because the other is not possible
  -- because the non-existing user cannot have a valid session
  (owner, _, _) <- createTeam domain 0
  user <- I.createUser domain def >>= getJSON 201
  email <- user %. "email" >>= asString
  inv <- postInvitation owner (PostInvitation (Just email) Nothing) >>= getJSON 201
  code <- I.getInvitationCode owner inv >>= getJSON 200 >>= (%. "code") & asString
  let body =
        def
          { name = Just email,
            email = Just email,
            password = Just defPassword,
            teamCode = Just code
          }
  addUser domain body >>= assertStatus 409

testTeamUserCannotBeInvited :: (HasCallStack) => App ()
testTeamUserCannotBeInvited = do
  (_, _, tm : _) <- createTeam OwnDomain 2
  (owner2, _, _) <- createTeam OwnDomain 0
  email <- tm %. "email" >>= asString
  postInvitation owner2 (PostInvitation (Just email) Nothing) >>= assertStatus 409

testUpgradePersonalToTeam :: (HasCallStack) => App ()
testUpgradePersonalToTeam = do
  alice <- randomUser OwnDomain def
  email <- alice %. "email" >>= asString
  let teamName = "wonderland"
  token <- Nginz.login OwnDomain email defPassword >>= getJSON 200 >>= (%. "access_token") & asString
  tid <- bindResponse (Nginz.upgradePersonalToTeam alice token teamName) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "team_name" `shouldMatch` teamName
    resp.json %. "team_id"

  alice' <- getUser alice alice >>= getJSON 200
  alice' %. "team" `shouldMatch` tid

  team <- getTeam alice tid >>= getJSON 200
  team %. "name" `shouldMatch` teamName

  iTeam <- asString tid >>= I.getTeam alice >>= getJSON 200
  iTeam %. "team.name" `shouldMatch` teamName
  iTeam %. "status" `shouldMatch` "active"

  bindResponse (getTeamMembers alice tid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    owner <- asList (resp.json %. "members") >>= assertOne
    owner %. "user" `shouldMatch` (alice %. "id")
    shouldBeNull $ owner %. "created_at"
    shouldBeNull $ owner %. "created_by"

  mem <- createTeamMember alice' def
  I.refreshIndex OwnDomain

  bindResponse (searchTeamAll alice') $ \resp -> do
    resp.status `shouldMatchInt` 200
    docs <- resp.json %. "documents" >>= asList
    actualIds <- for docs ((%. "id") >=> asString)
    expectedIds <- for [alice', mem] ((%. "id") >=> asString)
    actualIds `shouldMatchSet` expectedIds

testUpgradePersonalToTeamAlreadyInATeam :: (HasCallStack) => App ()
testUpgradePersonalToTeamAlreadyInATeam = do
  (alice, _, _) <- createTeam OwnDomain 0

  bindResponse (upgradePersonalToTeam alice "wonderland") $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "user-already-in-a-team"

testUpgradePersonalToTeamEmailDomainForAnotherBackend :: (HasCallStack) => App ()
testUpgradePersonalToTeamEmailDomainForAnotherBackend = forM_ [ExplicitVersion 8, Versioned] \version -> do
  domain <- randomDomain
  let email = "alice@" <> domain
  alice <- randomUser OwnDomain def {I.email = Just email}

  I.domainRegistrationPreAuthorize OwnDomain domain >>= assertStatus 204
  setup <- setupOwnershipTokenForBackend OwnDomain domain

  -- [customer admin] post no-registration config
  updateDomainRedirect
    OwnDomain
    version
    domain
    (Just setup.ownershipToken)
    (mkDomainRedirectBackend version "https://example.com" "https://webapp.example.com")
    >>= assertStatus 200

  bindResponse (upgradePersonalToTeam alice "wonderland") $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "condition-failed"

-- for additional tests of the CSV download particularly with SCIM users, please refer to 'Test.Spar.Scim.UserSpec'
testTeamMemberCsvExport :: (HasCallStack) => App ()
testTeamMemberCsvExport = do
  (owner, tid, members) <- createTeam OwnDomain 5

  modifiedMembers <- for
    ( zip
        ([0, 1, 2] <> repeat 0)
        (owner : members)
    )
    $ \(n, m) -> do
      handle <- randomHandle
      putHandle m handle >>= assertSuccess
      clients <-
        replicateM n
          $ addClient m def
          >>= getJSON 201
          >>= (%. "id")
          >>= asString
      for_ (listToMaybe clients) $ \c ->
        getNotifications m def {client = Just c}
      void $ I.putSSOId m def {I.scimExternalId = Just "foo"} >>= getBody 200
      setField "handle" handle m
        >>= setField "role" (if m == owner then "owner" else "member")
        >>= setField "num_clients" n

  memberMap :: Map.Map String Value <- fmap Map.fromList $ for (modifiedMembers) $ \m -> do
    uid <- m %. "id" & asString
    pure (uid, m)

  bindResponse (getTeamMembersCsv owner tid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    let rows = sort $ tail $ B8.lines $ resp.body
    length rows `shouldMatchInt` 5
    for_ rows $ \row -> do
      let cols = B8.split ',' row
      let uid = read $ B8.unpack $ cols !! 11
      let mem = memberMap Map.! uid

      ownerId <- owner %. "id" & asString
      let ownerMember = memberMap Map.! ownerId
      now <- formatTime defaultTimeLocale "%Y-%m-%d" <$> liftIO getCurrentTime
      numClients <- mem %. "num_clients" & asInt

      let parseField = unquote . read . B8.unpack . (cols !!)

      parseField 0 `shouldMatch` (mem %. "name")
      parseField 1 `shouldMatch` (mem %. "handle")
      parseField 2 `shouldMatch` (mem %. "email")
      role <- mem %. "role" & asString
      parseField 3 `shouldMatch` role
      when (role /= "owner") $ do
        take 10 (parseField 4) `shouldMatch` now
        parseField 5 `shouldMatch` (ownerMember %. "handle")
      parseField 7 `shouldMatch` "wire"
      parseField 9 `shouldMatch` "foo"
      parseField 12 `shouldMatch` show numClients
      (if numClients > 0 then shouldNotMatch else shouldMatch)
        (parseField 13)
        ""
      parseField 14 `shouldMatch` "active"
  where
    unquote :: String -> String
    unquote ('\'' : x) = x
    unquote x = x

testUpgradeGuestToTeamShouldFail :: (HasCallStack) => App ()
testUpgradeGuestToTeamShouldFail = do
  guest <- ephemeralUser OwnDomain

  upgradePersonalToTeam guest "wonderland" `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 404

testDeleteTeamUserRatelimitingIsPropagated :: (HasCallStack) => App ()
testDeleteTeamUserRatelimitingIsPropagated = do
  (owner, tid, mems) <- createTeam OwnDomain 10
  -- this is eventually going to run into rate limiting of internal request `/i/users/:uid/reauthenticate`
  statusCodes <- for mems $ \m -> do
    bindResponse (deleteTeamMember tid owner m) $ \resp -> do
      pure resp.status
  Set.fromList statusCodes `shouldMatchSet` ([202, 429] :: [Int])

testSelectTeamMembersByIds :: (HasCallStack) => App ()
testSelectTeamMembersByIds = do
  (owner, tid, mems) <- createTeam OwnDomain 5
  uids <- for (owner : mems) ((%. "id") >=> asString)

  selectTeamMembers OwnDomain tid uids `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    tms <- resp.json %. "members" >>= asList
    length tms `shouldMatchInt` 5

testListUsersEmailVisibility :: (HasCallStack) => App ()
testListUsersEmailVisibility = do
  let overrides :: ServiceOverrides =
        def {brigCfg = setField "optSettings.setEmailVisibility" "visible_to_self"}

  withModifiedBackend overrides $ \dom -> do
    (owner, _tid, mems) <- createTeam dom 5
    memQIds <- for mems objQidObject
    memEmails <- for mems ((%. "email") >=> asString)
    listUsers owner memQIds `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      returnedUsers <- resp.json %. "found" >>= asList
      returnedEmails <- for returnedUsers ((%. "email") >=> asString)
      returnedEmails `shouldMatchSet` memEmails

testListUsersContactStatus :: (HasCallStack) => App ()
testListUsersContactStatus = do
  (owner, tid, proteusUser : mlsUser : _) <- createTeam OwnDomain 3
  noClientUser <- randomUser OwnDomain def
  proteusClient <- addClient proteusUser def >>= getJSON 201
  mlsClient <- createMLSClient def mlsUser
  void $ uploadNewKeyPackage def mlsClient
  putUserSupportedProtocols mlsUser ["mls"] >>= assertSuccess
  configureMlsForTeam owner tid

  let users = [noClientUser, proteusUser, mlsUser]
  userIds <- for users objQidObject

  listUsers owner userIds `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    profiles <- resp.json %. "found" >>= asList
    for_ profiles $ \profile ->
      lookupField profile "contact_status" `shouldMatch` (Nothing :: Maybe Value)

  listUsersWithContactStatus owner userIds `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    profiles <- resp.json %. "found" >>= asList
    assertContactStatus profiles noClientUser "non-contactable"
    assertContactStatus profiles proteusUser "contactable"
    assertContactStatus profiles mlsUser "contactable"

  deleteClient proteusUser proteusClient >>= assertSuccess
  deleteClient mlsUser mlsClient.client >>= assertSuccess

  listUsersWithContactStatus owner userIds `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    profiles <- resp.json %. "found" >>= asList
    assertContactStatus profiles proteusUser "non-contactable"
    assertContactStatus profiles mlsUser "non-contactable"

-- | Only list-users with @include-contact-status=true@ returns
-- @contact_status@ fields; that endpoint is covered by its own tests. Other
-- (non-deprecated) endpoints returning user profiles must never set it.
--
-- Why do we need this test? Calculating the @contact_status@ is quite
-- expensive and shouldn't be done accidentally. Sharing response types might
-- make this easy, though.
testProfileEndpointsNoContactStatus :: (HasCallStack) => App ()
testProfileEndpointsNoContactStatus = do
  (owner, tid, _) <- createTeam OwnDomain 1
  alice <- randomUser OwnDomain def
  bob <- randomUser OwnDomain def

  getUser alice bob `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json `assertFieldMissing` "contact_status"

  void $ createApp owner tid def >>= getJSON 200
  getApps owner tid `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    profile <- assertOne =<< asList resp.json
    profile `assertFieldMissing` "contact_status"

configureMlsForTeam :: (HasCallStack, MakesValue owner) => owner -> String -> App ()
configureMlsForTeam owner tid =
  setTeamFeatureConfig
    owner
    tid
    "mls"
    ( object
        [ "status" .= ("enabled" :: String),
          "config"
            .= object
              [ "protocolToggleUsers" .= ([] :: [String]),
                "defaultProtocol" .= ("mls" :: String),
                "supportedProtocols" .= (["proteus", "mls"] :: [String]),
                "allowedCipherSuites" .= ([2] :: [Int]),
                "defaultCipherSuite" .= (2 :: Int)
              ]
        ]
    )
    >>= assertSuccess

assertContactStatus :: (HasCallStack) => [Value] -> Value -> String -> App ()
assertContactStatus profiles user expected =
  findProfile profiles user >>= \case
    Nothing -> assertFailure "list-users did not return the requested user"
    Just p -> p %. "contact_status.state" `shouldMatch` expected

findProfile :: (HasCallStack) => [Value] -> Value -> App (Maybe Value)
findProfile profiles user = do
  uid <- user %. "id"
  findM (fmap (== Just uid) . flip lookupField "id") profiles

keyPackageCount :: (HasCallStack) => ClientIdentity -> App Int
keyPackageCount c = countKeyPackages def c >>= getJSON 200 >>= (%. "count") >>= asInt

-- | Pending connections' @contact_status@ is supported for local and remote
-- targets (federated @get-users-contact-status@).
--
-- The requester lives on `OwnDomain``; the test runs once per `Domain`
-- constructor (`OwnDomain` or `OtherDomain`), with all targets on
-- @targetDomain@. 'OtherDomain' is a separate backend, so its users are remote
-- and their status can only come from a successful federation call.
testListUsersContactStatusScenarios :: (HasCallStack) => Domain -> App ()
testListUsersContactStatusScenarios targetDomain = do
  (owner, tid, _) <- createTeam OwnDomain 1
  configureMlsForTeam owner tid

  noClientUser <- randomUser targetDomain def
  proteusUser <- randomUser targetDomain def
  proteusClient <- addClient proteusUser def >>= getJSON 201 >>= (%. "id") >>= asString
  let mlsOnly = def {I.supportedProtocols = Just ["mls"]}
  mlsUser <- randomUser targetDomain mlsOnly
  mlsClient <- createMLSClient def mlsUser
  void $ uploadNewKeyPackage def mlsClient
  -- A key package for a suite that the requester team does not allow
  let disallowedSuite = Ciphersuite "0x0001"
  badSuiteUser <- randomUser targetDomain mlsOnly
  badSuiteClient <- createMLSClient def {ciphersuites = [disallowedSuite]} badSuiteUser
  void $ uploadNewKeyPackage disallowedSuite badSuiteClient

  userIds <- for [noClientUser, proteusUser, mlsUser, badSuiteUser] objQidObject
  prekeysBefore <- getClientPrekeyIds proteusUser proteusClient
  kpBefore <- keyPackageCount mlsClient

  listUsersWithContactStatus owner userIds `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    profiles <- resp.json %. "found" >>= asList
    assertContactStatus profiles noClientUser "non-contactable"
    assertContactStatus profiles proteusUser "contactable"
    assertContactStatus profiles mlsUser "contactable"
    assertContactStatus profiles badSuiteUser "non-contactable"

  -- Ensure that no key packages are consumed by the contact status check
  getClientPrekeyIds proteusUser proteusClient `shouldMatch` prekeysBefore
  kpAfter <- keyPackageCount mlsClient
  kpAfter `shouldMatchInt` kpBefore
  where
    getClientPrekeyIds user client =
      getClientPrekeys user client >>= getJSON 200 >>= asList >>= traverse asInt

-- | Remote backend is down: list-users still returns 200 and local users keep
-- their contact_status; the unreachable remote user is missing from the result
-- set.
testListUsersContactStatusRemoteDown :: (HasCallStack) => App ()
testListUsersContactStatusRemoteDown = do
  localUser <- randomUser OwnDomain def
  void $ addClient localUser def >>= getJSON 201
  (owner, _tid, _) <- createTeam OwnDomain 1
  downUser <- startDynamicBackends [mempty] \[downDomain] -> do
    u <- randomUser downDomain def
    void $ addClient u def >>= getJSON 201
    pure u
  -- backend is stopped again here
  userIds@[_, downUserId] <- for [localUser, downUser] objQidObject
  listUsersWithContactStatus owner userIds `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    profiles <- resp.json %. "found" >>= asList
    assertContactStatus profiles localUser "contactable"
    -- unreachable remote: no profile at all, reported as failed
    findProfile profiles downUser `shouldMatch` (Nothing :: Maybe Value)
    resp.json %. "failed" `shouldMatch` [downUserId]

-- | Remote backend without the contact-status endpoint (old federation
-- version): 200, profile returned, contact_status absent. Needs
-- ENABLE_FEDERATION_V1=1.
testListUsersContactStatusOldFedRemote :: (HasCallStack) => FedDomain 1 -> App ()
testListUsersContactStatusOldFedRemote fedDomain = do
  (owner, _tid, _) <- createTeam OwnDomain 1
  oldUser <- randomUser fedDomain def
  void $ addClient oldUser def >>= getJSON 201
  listUsersWithContactStatus owner [oldUser] `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    profile <- assertOne =<< asList (resp.json %. "found")
    lookupField profile "contact_status" `shouldMatch` (Nothing :: Maybe Value)

testGetTeamsInvitationInfo :: (HasCallStack) => App ()
testGetTeamsInvitationInfo = do
  (owner, tid, _) <- createTeam OwnDomain 1
  email <- randomEmail
  inv <- postInvitation owner (PostInvitation (Just email) Nothing) >>= getJSON 201
  code <- I.getInvitationCode owner inv >>= getJSON 200 >>= (%. "code") & asString
  bindResponse (getInvitationByCode owner code) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "email" `shouldMatch` email
    resp.json %. "team" `shouldMatch` tid
    resp.json %. "role" `shouldMatch` "member"
    resp.json %. "id" `shouldMatch` (inv %. "id")
    lookupField resp.json "managed_by" `shouldMatch` (Nothing :: Maybe Value)

testResendVerificationEmailPendingEmail :: (HasCallStack) => App ()
testResendVerificationEmailPendingEmail = do
  (owner, tid, tmem : _) <- createTeam OwnDomain 2
  newEmail <- randomEmail
  putUserEmail owner tmem newEmail >>= assertStatus 200
  uid <- asString $ tmem %. "id"
  putResendTeamMemberVerificationEmail owner tid uid >>= assertStatus 202

testResendVerificationEmailNoPendingEmail :: (HasCallStack) => App ()
testResendVerificationEmailNoPendingEmail = do
  (owner, tid, _) <- createTeam OwnDomain 1
  ownerUid <- asString $ owner %. "id"
  putResendTeamMemberVerificationEmail owner tid ownerUid >>= assertStatus 204

testResendVerificationEmailNonAdmin :: (HasCallStack) => App ()
testResendVerificationEmailNonAdmin = do
  (owner, tid, tmem : otherMember : _) <- createTeam OwnDomain 3
  newEmail <- randomEmail
  putUserEmail owner tmem newEmail >>= assertStatus 200
  memberUid <- asString $ tmem %. "id"
  putResendTeamMemberVerificationEmail otherMember tid memberUid >>= assertStatus 403

testResendVerificationEmailWrongTeamCaller :: (HasCallStack) => App ()
testResendVerificationEmailWrongTeamCaller = do
  (_, tid, tmem : _) <- createTeam OwnDomain 2
  (foreignOwner, _, _) <- createTeam OwnDomain 0
  memberUid <- asString $ tmem %. "id"
  putResendTeamMemberVerificationEmail foreignOwner tid memberUid >>= assertStatus 403

testResendVerificationEmailTargetNotInTeam :: (HasCallStack) => App ()
testResendVerificationEmailTargetNotInTeam = do
  (owner, tid, _) <- createTeam OwnDomain 1
  (foreignUser, _, _) <- createTeam OwnDomain 0
  foreignUid <- asString $ foreignUser %. "id"
  putResendTeamMemberVerificationEmail owner tid foreignUid >>= assertStatus 404

testResendVerificationEmailUserNotFound :: (HasCallStack) => App ()
testResendVerificationEmailUserNotFound = do
  (owner, tid, _) <- createTeam OwnDomain 1
  nonExistentUid <- liftIO (UUID.toString <$> UUID.nextRandom)
  putResendTeamMemberVerificationEmail owner tid nonExistentUid >>= assertStatus 404

testResendVerificationEmailScimUser :: (HasCallStack) => App ()
testResendVerificationEmailScimUser = do
  (owner, tid, _) <- createTeam OwnDomain 1
  tok <- createScimToken owner def >>= getJSON 200 >>= (%. "token") >>= asString
  scimUser <- randomScimUser
  uid <- createScimUser OwnDomain tok scimUser >>= getJSON 201 >>= (%. "id") >>= asString
  putResendTeamMemberVerificationEmail owner tid uid >>= assertStatus 204

testResendVerificationEmailScimUserWithSamlEmail :: (HasCallStack) => App ()
testResendVerificationEmailScimUserWithSamlEmail = do
  (owner, tid, _) <- createTeam OwnDomain 1
  void $ I.setTeamFeatureStatus OwnDomain tid "sso" "enabled"
  void $ I.setTeamFeatureStatus OwnDomain tid "validateSAMLemails" "enabled"
  (idp, _) <- registerTestIdPWithMetaWithPrivateCreds owner
  idpId <- asString $ idp.json %. "id"
  tok <- createScimToken owner def {idp = Just idpId} >>= getJSON 200 >>= (%. "token") >>= asString
  scimUser <- randomScimUser
  email <- scimUser %. "emails" >>= asList >>= assertOne >>= (%. "value") >>= asString
  uid <- createScimUser OwnDomain tok scimUser >>= getJSON 201 >>= (%. "id") >>= asString
  putResendTeamMemberVerificationEmail owner tid uid >>= assertStatus 202
  activateEmail OwnDomain email
  putResendTeamMemberVerificationEmail owner tid uid >>= assertStatus 204
