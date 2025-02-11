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

  updateTeamInvite owner domain (object ["team_invite" .= "allowed", "sso" .= idp])
    >>= assertStatus 200

  let email = "user@" <> domain
  addUser OwnDomain def {email = Just email} `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "condition-failed"

  -- Ensure that invitation to other teams still work because `team_invite` is set to `allowed`
  (owner2, _, _) <- createTeam OwnDomain 1
  invitationToTeam2 <- postInvitation owner2 def {email = Just email} >>= getJSON 201
  invitationCode <- (getInvitationCode owner2 invitationToTeam2 >>= getJSON 200) %. "code" & asString
  let body =
        def
          { email = Just email,
            teamCode = Just invitationCode
          }
  addUser owner2 body >>= assertSuccess
