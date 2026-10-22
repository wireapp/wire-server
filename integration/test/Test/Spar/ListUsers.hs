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

-- | Tests for listing SCIM users (@GET /scim/v2/Users@) without a filter, using
-- @startIndex@ and @count@. Listing is backed by an offset-paginated query in
-- brig which is only available when the user store is Postgres.
module Test.Spar.ListUsers where

import API.Spar
import GHC.Stack
import SetupHelpers
import Testlib.Prelude

-- | The storage location of users, as configured for brig.
getUserStore :: (HasCallStack) => App String
getUserStore = readServiceConfig Brig %. "postgresMigration.user" & asString

-- | Create a SCIM user. If 'acceptInvitation' is set, the invitation is accepted
-- immediately, otherwise brig would garbage collect the user because of the
-- very short team invitation timeout in the test environment.
createScimUserInTeam :: (HasCallStack, MakesValue domain) => domain -> String -> String -> Bool -> App String
createScimUserInTeam domain tok tid acceptInvitation = do
  scimUser <- randomScimUser
  email <- scimUser %. "emails.0.value" & asString
  scimId <- createScimUser domain tok scimUser >>= getJSON 201 >>= (%. "id") >>= asString
  when acceptInvitation $ registerInvitedUser domain tid email
  pure scimId

-- | Create a team with a SCIM token and @n@ active SCIM users. Returns the
-- token, the team ID, the owner and the SCIM IDs of the created users.
setupTeamWithScimUsers :: (HasCallStack, MakesValue domain) => domain -> Int -> App (String, String, Value, [String])
setupTeamWithScimUsers domain n = do
  (owner, tid, _) <- createTeam domain 1
  tok <- createScimToken owner def >>= getJSON 200 >>= (%. "token") >>= asString
  scimIds <- replicateM n $ createScimUserInTeam domain tok tid True
  pure (tok, tid, owner, scimIds)

-- | Assert on the page structure and return the SCIM IDs of the resources.
assertPage ::
  (HasCallStack, MakesValue domain) =>
  domain ->
  Maybe Int ->
  Maybe Int ->
  -- | expected startIndex
  Int ->
  -- | expected totalResults
  Int ->
  -- | expected itemsPerPage
  Int ->
  String ->
  App [String]
assertPage domain mStartIndex mCount expectedStart expectedTotal expectedItems tok =
  listScimUsersPaginate domain tok mStartIndex mCount `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "startIndex" `shouldMatchInt` expectedStart
    resp.json %. "totalResults" `shouldMatchInt` expectedTotal
    resp.json %. "itemsPerPage" `shouldMatchInt` expectedItems
    resources <- resp.json %. "Resources" & asList
    length resources `shouldMatchInt` expectedItems
    for resources $ \r -> r %. "id" & asString

testSparScimListUsersPaginated :: (HasCallStack) => App ()
testSparScimListUsersPaginated = do
  store <- getUserStore
  -- Only the Postgres user store supports offset based pagination.
  when (store == "postgresql") $ do
    let settings =
          def
            { brigCfg =
                setField "optSettings.setTeamInvitationTimeout" (4 :: Int)
                  -- Keep expired accounts around so listing must filter them.
                  . setField "optSettings.setExpiredUserCleanupTimeout" (3600 :: Int)
            }
    withModifiedBackend settings $ \domain -> do
      let total = 12
          defaultCount = 10
      (tok, tid, _owner, scimIds) <- setupTeamWithScimUsers domain total
      (tokOther, _tidOther, _ownerOther, scimIdsOther) <- setupTeamWithScimUsers domain 2

      -- walk through all pages, none of the users should be repeated and the team
      -- owner (managed by wire) should not be listed.
      let count = 5
      pages <- forM [1, 6, 11] $ \startIndex ->
        assertPage domain (Just startIndex) (Just count) startIndex total (max 0 (min count (total - startIndex + 1))) tok
      let allIds = concat pages
      length allIds `shouldMatchInt` total
      allIds `shouldMatchSet` scimIds

      -- the default count is 10
      firstPage <- assertPage domain Nothing Nothing 1 total defaultCount tok
      firstPage `shouldMatchSet` take defaultCount allIds
      void $ assertPage domain (Just 11) Nothing 11 total (total - 10) tok

      -- count=0 is used to just get the total
      void $ assertPage domain Nothing (Just 0) 1 total 0 tok

      -- index after the last user
      void $ assertPage domain (Just (total + 1)) (Just count) (total + 1) total 0 tok

      -- indexes lower than 1 are treated as 1
      forM_ [0, -2] $ \startIndex -> do
        ids <- assertPage domain (Just startIndex) (Just count) 1 total count tok
        ids `shouldMatchSet` take count allIds

      -- other teams are not mixed up
      otherIds <- assertPage domain Nothing Nothing 1 2 2 tokOther
      otherIds `shouldMatchSet` scimIdsOther

      -- pending (not yet accepted) SCIM users are listed as well
      pendingId <- createScimUserInTeam domain tok tid False
      idsWithPending <- assertPage domain (Just 1) (Just 20) 1 (total + 1) (total + 1) tok
      idsWithPending `shouldContain` [pendingId]

      -- Once the pending invitation expires, listing must exclude the user and
      -- stop counting it. Use only the listing endpoint so a GET by ID cannot
      -- trigger cleanup. Retry until expiration, but check the total outside the
      -- retry so asynchronous deletion cannot mask an inconsistent response.
      pageAfterExpiration <- retryT $ do
        page <- listScimUsersPaginate domain tok (Just 1) (Just 20) >>= getJSON 200
        resources <- page %. "Resources" & asList
        idsAfterExpiration <- for resources $ \r -> r %. "id" & asString
        idsAfterExpiration `shouldMatchSet` scimIds
        pure page
      pageAfterExpiration %. "totalResults" `shouldMatchInt` total
      pageAfterExpiration %. "itemsPerPage" `shouldMatchInt` total
      -- The count-only query must exclude the expired user as well.
      void $ assertPage domain Nothing (Just 0) 1 total 0 tok

      -- Expiration must not leave holes in pages or shift the accepted users.
      pagesAfterExpiration <- forM [1, 6, 11] $ \startIndex ->
        assertPage domain (Just startIndex) (Just count) startIndex total (max 0 (min count (total - startIndex + 1))) tok
      concat pagesAfterExpiration `shouldMatch` allIds

testSparScimListUsersUnsupportedStore :: (HasCallStack) => App ()
testSparScimListUsersUnsupportedStore = do
  store <- getUserStore
  when (store /= "postgresql") $ do
    (tok, _tid, _owner, _scimIds) <- setupTeamWithScimUsers OwnDomain 1
    listScimUsersPaginate OwnDomain tok (Just 1) (Just 10) `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 400
      resp.json %. "scimType" `shouldMatch` "tooMany"
