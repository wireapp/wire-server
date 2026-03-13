-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Spar.GetByEmail where

import API.BrigInternal (CreateUser (..))
import API.GalleyInternal
import API.Spar
import GHC.Stack
import qualified SAML2.WebSSO.Test.Util as SAML
import SetupHelpers
import Testlib.Prelude

-- | Test the /sso/get-by-email endpoint with multi-ingress setup
testGetSsoCodeByEmailWithMultiIngress ::
  (HasCallStack) =>
  TaggedBool "requireExternalEmailVerification" ->
  TaggedBool "idpScimToken" ->
  App ()
testGetSsoCodeByEmailWithMultiIngress (TaggedBool requireExternalEmailVerification) (TaggedBool isIdPScimToken) = do
  let ernieZHost = "nginz-https.ernie.example.com"
      bertZHost = "nginz-https.bert.example.com"

  withModifiedBackend
    def
      { sparCfg =
          setField "enableIdPByEmailDiscovery" True
            >=> removeField "saml.spSsoUri"
            >=> removeField "saml.spAppUri"
            >=> removeField "saml.contacts"
            >=> setField
              "saml.spDomainConfigs"
              ( object
                  [ ernieZHost
                      .= object
                        [ "spAppUri" .= ("https://webapp." ++ ernieZHost),
                          "spSsoUri" .= ("https://" ++ ernieZHost ++ "/sso"),
                          "contacts" .= [object ["type" .= ("ContactTechnical" :: String)]]
                        ],
                    bertZHost
                      .= object
                        [ "spAppUri" .= ("https://webapp." ++ bertZHost),
                          "spSsoUri" .= ("https://" ++ bertZHost ++ "/sso"),
                          "contacts" .= [object ["type" .= ("ContactTechnical" :: String)]]
                        ]
                  ]
              )
      }
    $ \domain -> do
      (owner, tid, _) <- createTeam domain 1
      assertSuccess =<< setTeamFeatureStatus domain tid "sso" "enabled"

      -- The test should work for both: SCIM user with and without email confirmation
      let status = if requireExternalEmailVerification then "enabled" else "disabled"
      assertSuccess =<< setTeamFeatureStatus owner tid "validateSAMLemails" status

      -- Create IdP for ernie domain
      SAML.SampleIdP idpmetaErnie _ _ _ <- SAML.makeSampleIdPMetadata
      idpIdErnie <-
        createIdpWithZHost owner (Just ernieZHost) idpmetaErnie `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.json %. "extraInfo.domain" `shouldMatch` ernieZHost
          resp.json %. "id" >>= asString

      -- Create IdP for bert domain
      SAML.SampleIdP idpmetaBert _ _ _ <- SAML.makeSampleIdPMetadata
      idpIdBert <-
        createIdpWithZHost owner (Just bertZHost) idpmetaBert `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.json %. "extraInfo.domain" `shouldMatch` bertZHost
          resp.json %. "id" >>= asString

      -- Create a SCIM user managed by the ernie IdP
      scimUser <- randomScimUser
      userEmail <-
        scimUser %. "emails" >>= asList >>= \case
          (e : _) -> e %. "value" >>= asString
          [] -> assertFailure "Expected at least one email"

      let idpTokenConfig = if isIdPScimToken then (def {idp = Just idpIdErnie}) else def
      scimTok <- createScimToken owner idpTokenConfig
      scimToken <- scimTok.json %. "token" & asString

      createScimUser domain scimToken scimUser >>= assertSuccess

      if isIdPScimToken
        then when requireExternalEmailVerification $ do
          -- Activate the email so the user can be found by email
          activateEmail domain userEmail
        else
          -- Team members get an invitation (not an activation mail)
          registerInvitedUser domain tid userEmail

      -- Get the SSO code by email with matching Z-Host (ernie)
      getSsoCodeByEmailWithZHost domain (Just ernieZHost) userEmail `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        ssoCodeStr <- resp.json %. "sso_code" >>= asString
        ssoCodeStr `shouldMatch` idpIdErnie

      -- Get the SSO code by email without Z-Host (should return 404 with null - multiple IdPs)
      getSsoCodeByEmail domain userEmail `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 404
        mbSsoCode <- lookupField resp.json "sso_code"
        mbSsoCode `shouldMatch` (Nothing :: Maybe Value)

      -- Get the SSO code by email with Z-Host for bert domain (should return bert's IdP)
      getSsoCodeByEmailWithZHost domain (Just bertZHost) userEmail `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        ssoCodeStr <- resp.json %. "sso_code" >>= asString
        ssoCodeStr `shouldMatch` idpIdBert

-- | Test the /sso/get-by-email endpoint with regular (non-multi-ingress) setup
testGetSsoCodeByEmailRegular :: (HasCallStack) => (TaggedBool "requireExternalEmailVerification") -> (TaggedBool "idpScimToken") -> App ()
testGetSsoCodeByEmailRegular (TaggedBool requireExternalEmailVerification) (TaggedBool isIdPScimToken) =
  withModifiedBackend def {sparCfg = setField "enableIdPByEmailDiscovery" True}
    $ \domain -> do
      (owner, tid, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner tid "sso" "enabled"

      -- The test should work for both: SCIM user with and without email confirmation
      let status = if requireExternalEmailVerification then "enabled" else "disabled"
      assertSuccess =<< setTeamFeatureStatus owner tid "validateSAMLemails" status

      -- Create IdP without domain binding
      SAML.SampleIdP idpmeta _ _ _ <- SAML.makeSampleIdPMetadata
      idpId <-
        createIdp owner idpmeta `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.json %. "id" >>= asString

      -- Create a SCIM user
      scimUser <- randomScimUser
      userEmail <-
        scimUser %. "emails" >>= asList >>= \case
          (e : _) -> e %. "value" >>= asString
          [] -> assertFailure "Expected at least one email"

      let idpTokenConfig = if isIdPScimToken then (def {idp = Just idpId}) else def
      scimTok <- createScimToken owner idpTokenConfig
      scimToken <- scimTok.json %. "token" & asString

      createScimUser domain scimToken scimUser >>= assertSuccess

      if isIdPScimToken
        then when requireExternalEmailVerification $ do
          -- Activate the email so the user can be found by email
          activateEmail domain userEmail
        else
          -- Team members get an invitation (not an activation mail)
          registerInvitedUser domain tid userEmail

      -- Get the SSO code by email
      getSsoCodeByEmail domain userEmail `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        ssoCodeStr <- resp.json %. "sso_code" >>= asString
        ssoCodeStr `shouldMatch` idpId

-- | Test that non-SCIM users get no SSO code
testGetSsoCodeByEmailNonScimUser :: (HasCallStack) => App ()
testGetSsoCodeByEmailNonScimUser = do
  withModifiedBackend
    def {sparCfg = setField "enableIdPByEmailDiscovery" True}
    $ \domain -> do
      (owner, tid, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner tid "sso" "enabled"

      -- Create IdP without domain binding
      SAML.SampleIdP idpmeta _ _ _ <- SAML.makeSampleIdPMetadata
      createIdp owner idpmeta >>= assertSuccess

      -- Register user
      usr <- randomUser domain def {activate = True}
      userEmail <- usr %. "email" & asString

      -- Try to get SSO code for regular (non-SCIM) user - should return 404 with null
      getSsoCodeByEmail domain userEmail `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 404
        mbSsoCode <- lookupField resp.json "sso_code"
        mbSsoCode `shouldMatch` (Nothing :: Maybe Value)

-- | Test that the endpoint returns 404 with null when the feature is disabled (regular setup)
testGetSsoCodeByEmailDisabledRegular :: (HasCallStack) => App ()
testGetSsoCodeByEmailDisabledRegular = do
  withModifiedBackend
    def {sparCfg = setField "enableIdPByEmailDiscovery" False}
    $ \domain -> do
      (owner, tid, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner tid "sso" "enabled"

      -- Create IdP
      SAML.SampleIdP idpmeta _ _ _ <- SAML.makeSampleIdPMetadata
      idpId <-
        createIdp owner idpmeta `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.json %. "id" >>= asString

      -- Create a SCIM user
      scimUser <- randomScimUser
      userEmail <-
        scimUser %. "emails" >>= asList >>= \case
          (e : _) -> e %. "value" >>= asString
          [] -> assertFailure "Expected at least one email"

      scimTok <- createScimToken owner def {idp = Just idpId}
      scimToken <- scimTok.json %. "token" & asString

      createScimUser domain scimToken scimUser >>= assertSuccess

      -- Activate the email so the user can be found by email
      activateEmail domain userEmail

      -- With feature disabled, should return 404 with empty ssoCode
      getSsoCodeByEmail domain userEmail `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 404
        mbSsoCode <- lookupField resp.json "sso_code"
        mbSsoCode `shouldMatch` (Nothing :: Maybe Value)

-- | Test that the endpoint returns 404 with null when the feature is disabled (multi-ingress setup)
testGetSsoCodeByEmailDisabledMultiIngress :: (HasCallStack) => App ()
testGetSsoCodeByEmailDisabledMultiIngress = do
  let ernieZHost = "nginz-https.ernie.example.com"
      bertZHost = "nginz-https.bert.example.com"

  withModifiedBackend
    def
      { sparCfg =
          setField "enableIdPByEmailDiscovery" False
            >=> removeField "saml.spSsoUri"
            >=> removeField "saml.spAppUri"
            >=> removeField "saml.contacts"
            >=> setField
              "saml.spDomainConfigs"
              ( object
                  [ ernieZHost
                      .= object
                        [ "spAppUri" .= ("https://webapp." ++ ernieZHost),
                          "spSsoUri" .= ("https://" ++ ernieZHost ++ "/sso"),
                          "contacts" .= [object ["type" .= ("ContactTechnical" :: String)]]
                        ],
                    bertZHost
                      .= object
                        [ "spAppUri" .= ("https://webapp." ++ bertZHost),
                          "spSsoUri" .= ("https://" ++ bertZHost ++ "/sso"),
                          "contacts" .= [object ["type" .= ("ContactTechnical" :: String)]]
                        ]
                  ]
              )
      }
    $ \domain -> do
      (owner, tid, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner tid "sso" "enabled"

      -- Create IdP for ernie domain
      SAML.SampleIdP idpmetaErnie _ _ _ <- SAML.makeSampleIdPMetadata
      idpIdErnie <-
        createIdpWithZHost owner (Just ernieZHost) idpmetaErnie `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.json %. "extraInfo.domain" `shouldMatch` ernieZHost
          resp.json %. "id" >>= asString

      -- Create a SCIM user
      scimUser <- randomScimUser
      userEmail <-
        scimUser %. "emails" >>= asList >>= \case
          (e : _) -> e %. "value" >>= asString
          [] -> assertFailure "Expected at least one email"

      scimTok <- createScimToken owner def {idp = Just idpIdErnie}
      scimToken <- scimTok.json %. "token" & asString

      createScimUser domain scimToken scimUser >>= assertSuccess

      -- Activate the email so the user can be found by email
      activateEmail domain userEmail

      -- With feature disabled, should return 404 with null even with valid IdP
      bindResponse (getSsoCodeByEmailWithZHost domain (Just ernieZHost) userEmail) $ \resp -> do
        resp.status `shouldMatchInt` 404
        mbSsoCode <- lookupField resp.json "sso_code"
        mbSsoCode `shouldMatch` (Nothing :: Maybe Value)
