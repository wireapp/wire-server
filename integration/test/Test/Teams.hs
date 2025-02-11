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
import API.Galley (getTeam, getTeamMembers, getTeamMembersCsv, getTeamNotifications)
import qualified API.GalleyInternal as I
import API.Gundeck
import qualified API.Nginz as Nginz
import Control.Monad.Codensity (Codensity (runCodensity))
import Control.Monad.Extra (findM)
import Control.Monad.Reader (asks)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import Data.Time.Clock
import Data.Time.Format
import Notifications
import SetupHelpers
import Testlib.JSON
import Testlib.Prelude
import Testlib.ResourcePool (acquireResources)

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
        newUserInvUrl `shouldContainString` "/register"

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
