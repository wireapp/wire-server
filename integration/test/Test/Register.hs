{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Test.Register where

import API.Brig
import API.BrigInternal
import API.Common
import API.GalleyInternal (setTeamFeatureLockStatus, setTeamFeatureStatus)
import SetupHelpers
import Testlib.Prelude

testDisallowRegistrationWhenEmailDomainIsClaimedByOtherBackend :: (HasCallStack) => App ()
testDisallowRegistrationWhenEmailDomainIsClaimedByOtherBackend = do
  domain <- randomDomain
  setup <- setupOwnershipToken OwnDomain domain

  -- [backoffice] preauth
  domainRegistrationPreAuthorize OwnDomain domain >>= assertStatus 204

  -- [customer admin] post no-registration config
  updateDomainRedirect
    OwnDomain
    domain
    (Just setup.ownershipToken)
    (object ["domain_redirect" .= "backend", "backend_url" .= "https://example.com"])
    >>= assertStatus 200

  let email = "user@" <> domain
  addUser OwnDomain def {email = Just email} `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "condition-failed"

testDisallowRegistrationWhenEmailDomainDoesNotAllowRegistration :: (HasCallStack) => App ()
testDisallowRegistrationWhenEmailDomainDoesNotAllowRegistration = do
  domain <- randomDomain
  setup <- setupOwnershipToken OwnDomain domain

  -- [backoffice] preauth
  domainRegistrationPreAuthorize OwnDomain domain >>= assertStatus 204

  -- [customer admin] post no-registration config
  updateDomainRedirect
    OwnDomain
    domain
    (Just setup.ownershipToken)
    (object ["domain_redirect" .= "no-registration"])
    >>= assertStatus 200

  let email = "user@" <> domain
  addUser OwnDomain def {email = Just email} `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "condition-failed"

  addUser OwnDomain def {email = Just email, newTeamName = Just "new test team"} `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "condition-failed"

testAllowRegistrationWhenEmailDomainIsTakenByATeamButRedirectIsNone :: (HasCallStack) => App ()
testAllowRegistrationWhenEmailDomainIsTakenByATeamButRedirectIsNone = do
  (owner, tid, _) <- createTeam OwnDomain 2

  -- enable domain registration feature
  assertSuccess =<< do
    setTeamFeatureLockStatus owner tid "domainRegistration" "unlocked"
    setTeamFeatureStatus owner tid "domainRegistration" "enabled"

  domain <- randomDomain
  setup <- setupOwnershipToken OwnDomain domain
  authorizeTeam owner domain setup.ownershipToken >>= assertStatus 200

  -- [customer admin] post no-registration config
  updateTeamInvite
    owner
    domain
    (object ["team_invite" .= "team", "team" .= tid])
    >>= assertStatus 200

  let email = "user@" <> domain
  addUser OwnDomain def {email = Just email} >>= assertSuccess

testDisallowRegistrationWhenEmailDomainIsTakenByATeamWithSSO :: (HasCallStack) => App ()
testDisallowRegistrationWhenEmailDomainIsTakenByATeamWithSSO = do
  (owner, tid, _) <- createTeam OwnDomain 1

  -- enable domain registration feature
  assertSuccess =<< do
    setTeamFeatureLockStatus owner tid "domainRegistration" "unlocked"
    setTeamFeatureStatus owner tid "domainRegistration" "enabled"

  domain <- randomDomain
  setup <- setupOwnershipToken OwnDomain domain
  authorizeTeam owner domain setup.ownershipToken >>= assertStatus 200

  void $ setTeamFeatureStatus owner tid "sso" "enabled"
  idp <- bindResponse (registerTestIdPWithMeta owner) $ \resp -> do
    resp.status `shouldMatchInt` 201
    resp.json %. "id"

  updateTeamInvite owner domain (object ["team_invite" .= "not-allowed", "sso" .= idp])
    >>= assertStatus 200

  let email = "user@" <> domain
  addUser OwnDomain def {email = Just email} `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "condition-failed"

  (owner2, _, _) <- createTeam OwnDomain 1

  -- Inviting a user to another team doesn't work
  postInvitation owner2 def {email = Just email} `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "condition-failed"

  -- Inviting a user to the same team also doesn't work
  postInvitation owner def {email = Just email} `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "condition-failed"

  updateTeamInvite owner domain (object ["team_invite" .= "allowed", "sso" .= idp])
    >>= assertStatus 200

  -- Now invitation to any teams works
  invitationToTeam2 <- postInvitation owner2 def {email = Just email} >>= getJSON 201
  invitationCodeToTeam2 <- (getInvitationCode owner2 invitationToTeam2 >>= getJSON 200) %. "code" & asString
  assertSuccess
    =<< addUser
      owner2
      def
        { email = Just email,
          teamCode = Just invitationCodeToTeam2
        }

  let email2 = "user2@" <> domain
  invitationToOrigTeam <- postInvitation owner def {email = Just email2} >>= getJSON 201
  invitationCodeToOrigTeam <- (getInvitationCode owner invitationToOrigTeam >>= getJSON 200) %. "code" & asString
  assertSuccess
    =<< addUser
      owner
      def
        { email = Just email2,
          teamCode = Just invitationCodeToOrigTeam
        }

  updateTeamInvite owner domain (object ["team_invite" .= "team", "sso" .= idp, "team" .= tid])
    >>= assertStatus 200

  -- Now invitaions only work for the orig team
  let email3 = "user3@" <> domain
  postInvitation owner2 def {email = Just email3} `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "condition-failed"

  invitation2ToOrigTeam <- postInvitation owner def {email = Just email3} >>= getJSON 201
  invitation2CodeToOrigTeam <- (getInvitationCode owner invitation2ToOrigTeam >>= getJSON 200) %. "code" & asString
  assertSuccess
    =<< addUser
      owner
      def
        { email = Just email3,
          teamCode = Just invitation2CodeToOrigTeam
        }
