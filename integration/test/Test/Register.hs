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
testDisallowRegistrationWhenEmailDomainIsClaimedByOtherBackend = forM_ [(ExplicitVersion 8), Versioned] \version -> do
  domain <- randomDomain
  domainRegistrationPreAuthorize OwnDomain domain >>= assertStatus 204
  setup <- setupOwnershipTokenForBackend OwnDomain domain

  -- [customer admin] post no-registration config
  updateDomainRedirect
    OwnDomain
    version
    domain
    (Just setup.ownershipToken)
    (mkDomainRedirectBackend version "https://example.com" "https://webapp.example.com")
    >>= assertStatus 200

  let email = "user@" <> domain
  addUser OwnDomain def {email = Just email} `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "condition-failed"

testDisallowRegistrationWhenEmailDomainDoesNotAllowRegistration :: (HasCallStack) => App ()
testDisallowRegistrationWhenEmailDomainDoesNotAllowRegistration = forM_ [(ExplicitVersion 8), Versioned] \version -> do
  domain <- randomDomain

  -- [backoffice] preauth
  domainRegistrationPreAuthorize OwnDomain domain >>= assertStatus 204
  setup <- setupOwnershipTokenForBackend OwnDomain domain

  -- [customer admin] post no-registration config
  updateDomainRedirect
    OwnDomain
    version
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
  setup <- setupOwnershipTokenForTeam owner domain
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
  setup <- setupOwnershipTokenForTeam owner domain
  authorizeTeam owner domain setup.ownershipToken >>= assertStatus 200

  void $ setTeamFeatureStatus owner tid "sso" "enabled"
  (idp, idpMeta) <- registerTestIdPWithMetaWithPrivateCreds owner
  idpId <- asString $ idp.json %. "id"

  updateTeamInvite owner domain (object ["team_invite" .= "not-allowed", "sso" .= idpId])
    >>= assertStatus 200

  let email = "user@" <> domain
  addUser OwnDomain def {email = Just email} `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "condition-failed"

  -- Registering with SSO works
  void $ loginWithSaml True tid ("sso@" <> domain) (idpId, idpMeta)

  (owner2, _, _) <- createTeam OwnDomain 1

  -- TODO: Do we have to block SSO and SCIM regsitrations for other teams here?

  -- Inviting a user to another team doesn't work
  postInvitation owner2 def {email = Just email} `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "condition-failed"

  -- Inviting a user to the same team also doesn't work
  postInvitation owner def {email = Just email} `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "condition-failed"

  updateTeamInvite owner domain (object ["team_invite" .= "allowed", "sso" .= idpId])
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

  updateTeamInvite owner domain (object ["team_invite" .= "team", "sso" .= idpId, "team" .= tid])
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

testDisallowAcceptingInvitesAfterDomainIsClaimed :: (HasCallStack) => App ()
testDisallowAcceptingInvitesAfterDomainIsClaimed = forM_ [(ExplicitVersion 8), Versioned] \version -> do
  domain <- randomDomain
  (owner, _, _) <- createTeam OwnDomain 1
  let email = "user@" <> domain
  invitation <- postInvitation owner def {email = Just email} >>= getJSON 201
  invitationCode <- (getInvitationCode owner invitation >>= getJSON 200) %. "code" & asString

  -- [backoffice] preauth
  domainRegistrationPreAuthorize OwnDomain domain >>= assertStatus 204
  setup <- setupOwnershipTokenForBackend OwnDomain domain

  -- [customer admin] post no-registration config
  updateDomainRedirect
    OwnDomain
    version
    domain
    (Just setup.ownershipToken)
    (mkDomainRedirectBackend version "https:/example.com" "https://webapp.example.com")
    >>= assertStatus 200

  addUser owner def {email = Just email, teamCode = Just invitationCode} `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "condition-failed"
