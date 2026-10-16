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

module Test.Spar.ExtendedNameIdAttributes where

import API.BrigInternal (getUsersId)
import API.Common (randomHandle)
import API.GalleyInternal (setTeamFeatureStatus)
import Data.Text (Text, pack)
import qualified SAML2.WebSSO as SAML
import qualified Text.XML.DSig as SAML
import SetupHelpers
import Testlib.Prelude

-- | Reproducer for WPB-1506: spar's lookup key for SAML users
-- (`normalized_uname_id` in spar.user_v2) ignores NameQualifier,
-- SPNameQualifier and SPProvidedID. Two NameIDs that differ only in those
-- fields are meant to be distinct subjects per the SAML spec, but spar
-- resolves them to the same identity.
--
-- 1. Log in with a bare NameID (no extended attributes) -> baseline user,
--    one row in spar.user_v2.
-- 2. Log in again with the *same* base identifier, but with the three
--    extended attributes set -> should be a different subject, but
--    resolves to the same uid instead.
testExtendedNameIdAttributesCollideWithBase :: (HasCallStack) => App ()
testExtendedNameIdAttributesCollideWithBase = do
  (tid, createIdpResp, idpId, idp) <- setupSamlTeam

  randomUsername <- randomHandle
  let baseNameId = mkNameId randomUsername Nothing Nothing Nothing
      extendedNameId = extendedAttributesNameId randomUsername

  -- Step 1: baseline login, no extended attributes. Creates the user.
  (mUidBase, _) <- loginWithSaml True tid baseNameId (idpId, idp)
  uidBase <- assertJust "expected baseline NameID login to succeed" mUidBase

  -- Step 2: same base identifier, now with extended attributes set. Per
  -- SAML this is a different subject; spar's normalized lookup key drops
  -- the qualifiers, so it incorrectly resolves to uidBase again instead of
  -- being rejected or creating a second, distinct identity.
  (mUidExtended, _) <- loginWithSaml True tid extendedNameId (idpId, idp)
  uidExtended <- assertJust "expected extended-attribute NameID login to succeed" mUidExtended

  uidExtended `shouldMatch` uidBase

  -- Confirm at the API/storage level too, not just via the cookie: brig's
  -- view of this user's sso_id/tenant still points at this IdP's issuer,
  -- i.e. there is exactly one identity on record for both NameIDs.
  issuer <- createIdpResp.json %. "metadata.issuer" >>= asString
  getUsersId OwnDomain [uidExtended] `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    ssoId <- resp.json %. "0.sso_id"
    ssoIdTenant <- ssoId %. "tenant" >>= asString
    ssoIdTenant `shouldContain` issuer

-- | Trigger for manual DB inspection: single login with a bare NameID (no
-- extended attributes). Prints identifiers to locate the rows.
testLoginNameIdWithoutAttributes :: (HasCallStack) => App ()
testLoginNameIdWithoutAttributes = do
  (tid, _, idpId, idp) <- setupSamlTeam
  username <- randomHandle
  let nameId = mkNameId username Nothing Nothing Nothing
  (mUid, _) <- loginWithSaml True tid nameId (idpId, idp)
  uid <- assertJust "expected NameID login without attributes to succeed" mUid
  putStrLn $ "WITHOUT attributes: tid=" <> tid <> " idp=" <> idpId <> " username=" <> username <> " uid=" <> uid

-- | Trigger for manual DB inspection: single login with NameQualifier,
-- SPNameQualifier and SPProvidedID set. Prints identifiers to locate the rows.
testLoginNameIdWithAttributes :: (HasCallStack) => App ()
testLoginNameIdWithAttributes = do
  (tid, _, idpId, idp) <- setupSamlTeam
  username <- randomHandle
  let nameId = extendedAttributesNameId username
  (mUid, _) <- loginWithSaml True tid nameId (idpId, idp)
  uid <- assertJust "expected NameID login with attributes to succeed" mUid
  putStrLn $ "WITH attributes: tid=" <> tid <> " idp=" <> idpId <> " username=" <> username <> " uid=" <> uid

-- | Fresh team with SSO enabled and one registered IdP.
setupSamlTeam :: (HasCallStack) => App (String, Response, String, (SAML.IdPMetadata, SAML.SignPrivCreds))
setupSamlTeam = do
  (owner, tid, []) <- createTeam OwnDomain 1
  void $ setTeamFeatureStatus owner tid "sso" "enabled"
  (createIdpResp, idp) <- registerTestIdPWithMetaWithPrivateCreds owner
  assertSuccess createIdpResp
  idpId <- asString =<< (createIdpResp.json %. "id")
  pure (tid, createIdpResp, idpId, idp)

mkNameId :: (HasCallStack) => String -> Maybe Text -> Maybe Text -> Maybe Text -> SAML.NameID
mkNameId username nameQ spNameQ spProvidedId =
  fromRight (error "could not create name id")
    $ SAML.mkNameID (SAML.mkUNameIDUnspecified (pack username)) nameQ spNameQ spProvidedId

extendedAttributesNameId :: (HasCallStack) => String -> SAML.NameID
extendedAttributesNameId username =
  mkNameId
    username
    (Just (pack "some-name-qualifier"))
    (Just (pack "some-sp-name-qualifier"))
    (Just (pack "some-sp-provided-id"))
