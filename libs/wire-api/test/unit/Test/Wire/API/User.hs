{-# LANGUAGE ScopedTypeVariables #-}

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

module Test.Wire.API.User (tests) where

import Control.Lens ((.~))
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.Types as Aeson
import Data.Domain
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (UserLegalHoldNoConsent))
import Data.Qualified
import Data.Schema (schemaIn)
import Data.UUID.V4 qualified as UUID
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Wire.API.Team.Member (TeamMember)
import Wire.API.Team.Member qualified as TeamMember
import Wire.API.Team.Role
import Wire.API.User

tests :: TestTree
tests =
  testGroup
    "User (types vs. aeson)"
    [ parseIdentityTests,
      jsonNullTests,
      testMkUserProfile
    ]

jsonNullTests :: TestTree
jsonNullTests = testGroup "JSON null" [testCase "userProfile" testUserProfile]

testMkUserProfile :: TestTree
testMkUserProfile =
  testGroup
    "mkUserProfile"
    [ testEmailVisibleToSelf,
      testEmailVisibleIfOnTeam,
      testEmailVisibleIfOnSameTeam
    ]

testEmailVisibleToSelf :: TestTree
testEmailVisibleToSelf =
  testProperty "should not contain email when email visibility is EmailVisibleToSelf" $
    \user lhStatus ->
      let profile = mkUserProfile EmailVisibleToSelf user lhStatus
       in profileEmail profile === Nothing
            .&&. profileLegalholdStatus profile === lhStatus

testEmailVisibleIfOnTeam :: TestTree
testEmailVisibleIfOnTeam =
  testProperty "should contain email only if the user has one and is part of a team when email visibility is EmailVisibleIfOnTeam" $
    \user lhStatus ->
      let profile = mkUserProfile EmailVisibleIfOnTeam user lhStatus
       in (profileEmail profile === (userTeam user *> userEmail user))
            .&&. profileLegalholdStatus profile === lhStatus

testEmailVisibleIfOnSameTeam :: TestTree
testEmailVisibleIfOnSameTeam =
  testGroup "when email visibility is EmailVisibleIfOnSameTeam" [testNoViewerTeam, testViewerDifferentTeam, testViewerSameTeamExternal, testViewerSameTeamNotExternal]
  where
    testNoViewerTeam = testProperty "should not contain email when viewer is not part of a team" $
      \user lhStatus ->
        let profile = mkUserProfile (EmailVisibleIfOnSameTeam Nothing) user lhStatus
         in (profileEmail profile === Nothing)
              .&&. profileLegalholdStatus profile === lhStatus

    testViewerDifferentTeam = testProperty "should not contain email when viewer is not part of the same team" $
      \viewerTeamId viewerMembership user lhStatus ->
        let profile = mkUserProfile (EmailVisibleIfOnSameTeam (Just (viewerTeamId, viewerMembership))) user lhStatus
         in Just viewerTeamId /= userTeam user ==>
              ( profileEmail profile === Nothing
                  .&&. profileLegalholdStatus profile === lhStatus
              )

    testViewerSameTeamExternal = testProperty "should not contain email when viewer is part of the same team and is an external member" $
      \viewerTeamId (viewerMembershipNoRole :: TeamMember) userNoTeam lhStatus ->
        let user = userNoTeam {userTeam = Just viewerTeamId}
            viewerMembership = viewerMembershipNoRole & TeamMember.permissions .~ TeamMember.rolePermissions RoleExternalPartner
            profile = mkUserProfile (EmailVisibleIfOnSameTeam (Just (viewerTeamId, viewerMembership))) user lhStatus
         in ( profileEmail profile === Nothing
                .&&. profileLegalholdStatus profile === lhStatus
            )

    testViewerSameTeamNotExternal = testProperty "should contain email when viewer is part of the same team and is not an external member" $
      \viewerTeamId (viewerMembershipNoRole :: TeamMember) viewerRole userNoTeam lhStatus ->
        let user = userNoTeam {userTeam = Just viewerTeamId}
            viewerMembership = viewerMembershipNoRole & TeamMember.permissions .~ TeamMember.rolePermissions viewerRole
            profile = mkUserProfile (EmailVisibleIfOnSameTeam (Just (viewerTeamId, viewerMembership))) user lhStatus
         in viewerRole /= RoleExternalPartner ==>
              ( profileEmail profile === userEmail user
                  .&&. profileLegalholdStatus profile === lhStatus
              )

testUserProfile :: Assertion
testUserProfile = do
  uid <- Id <$> UUID.nextRandom
  let domain = Domain "example.com"
  let colour = ColourId 0
  let userProfile = UserProfile (Qualified uid domain) (Name "name") Nothing (Pict []) [] colour False Nothing Nothing Nothing Nothing Nothing UserLegalHoldNoConsent defSupportedProtocols
  let profileJSONAsText = show $ Aeson.encode userProfile
  let msg = "toJSON encoding must not convert Nothing to null, but instead omit those json fields for backwards compatibility. UserProfileJSON:" <> profileJSONAsText
  assertBool msg (not $ "null" `isInfixOf` profileJSONAsText)

parseIdentityTests :: TestTree
parseIdentityTests =
  let (=#=) :: Either String (Maybe UserIdentity) -> [Pair] -> Assertion
      (=#=) uid (object -> Object obj) = assertEqual "=#=" uid (parseEither (schemaIn maybeUserIdentityObjectSchema) obj)
      (=#=) _ bad = error $ "=#=: impossible: " <> show bad
   in testGroup
        "parseIdentity"
        [ testCase "EmailIdentity" $
            Right (Just (EmailIdentity hemail)) =#= [email],
          testCase "SSOIdentity" $ do
            Right (Just (SSOIdentity hssoid Nothing)) =#= [ssoid]
            Right (Just (SSOIdentity hssoid (Just hemail))) =#= [ssoid, email],
          testCase "Phone not part of identity any more" $
            Right Nothing =#= [badphone],
          testCase "Bad email" $
            Left "Error in $.email: Invalid email. Expected '<local>@<domain>'." =#= [bademail],
          testCase "Nothing" $
            Right Nothing =#= [("something_unrelated", "#")]
        ]
  where
    hemail = Email "me" "example.com"
    email = ("email", "me@example.com")
    bademail = ("email", "justme")
    badphone = ("phone", "__@@")
    hssoid = UserSSOId mkSimpleSampleUref
    ssoid = ("sso_id", toJSON hssoid)
