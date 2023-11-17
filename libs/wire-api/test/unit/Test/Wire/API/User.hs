{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}

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

module Test.Wire.API.User where

import Control.Lens (Lens', (?~), _1, _2, _3, _4, _5, _6)
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types qualified as Aeson
import Data.Domain
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (UserLegalHoldNoConsent))
import Data.Qualified
import Data.Schema (schema, schemaIn)
import Data.UUID.V4 qualified as UUID
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.User

tests :: TestTree
tests = testGroup "User (types vs. aeson)" unitTests

unitTests :: [TestTree]
unitTests = parseIdentityTests ++ jsonNullTests

jsonNullTests :: [TestTree]
jsonNullTests = [testGroup "JSON null" [testCase "userProfile" testUserProfile]]

testUserProfile :: Assertion
testUserProfile = do
  uid <- Id <$> UUID.nextRandom
  let domain = Domain "example.com"
  let colour = ColourId 0
  let userProfile = UserProfile (Qualified uid domain) (Name "name") (Pict []) [] colour False Nothing Nothing Nothing Nothing Nothing UserLegalHoldNoConsent defSupportedProtocols
  let profileJSONAsText = show $ Aeson.encode userProfile
  let msg = "toJSON encoding must not convert Nothing to null, but instead omit those json fields for backwards compatibility. UserProfileJSON:" <> profileJSONAsText
  assertBool msg (not $ "null" `isInfixOf` profileJSONAsText)

parseIdentityTests :: [TestTree]
parseIdentityTests =
  [ testGroup
      "parse UserIdentity: {Email,Phone,Full}Identity"
      [ testCase "FullIdentity" $
          [("email", "me@example.com"), ("phone", "+493012345678")]
            =#= Right (Just (FullIdentity (Email "me" "example.com") (Phone "+493012345678"))),
        testCase "EmailIdentity" $
          [("email", "me@example.com")]
            =#= Right (Just (EmailIdentity (Email "me" "example.com"))),
        testCase "PhoneIdentity" $
          [("phone", "+493012345678")]
            =#= Right (Just (PhoneIdentity (Phone "+493012345678"))),
        testCase "Bad phone" $
          [("phone", "__@@")]
            =#= Left "Error in $.phone: Invalid phone number. Expected E.164 format.",
        testCase "Bad email" $
          [("email", "justme")]
            =#= Left "Error in $.email: Invalid email. Expected '<local>@<domain>'.",
        testCase "Nothing" $
          [("something_unrelated", "#")]
            =#= Right Nothing
      ],
    testGroup
      "parse UAuthId"
      [ -- {}
        let jsonIn = [aesonQQ|{}|]
            err = "Error in $: key \"team\" not found"
         in mkUAuthIdTestCase "1" jsonIn err,
        -- {email, team}
        let jsonIn =
              [aesonQQ|{"email": {"email": "me@example.com", "source": "scim_emails"},
                        "team": "226923f0-6f15-11ee-96bd-33644427c814"}|]
            err = "Error in $: at least one of saml_id, scim_external_id must be present"
         in mkUAuthIdTestCase "2" jsonIn err,
        -- {eid, team}
        let jsonIn =
              [aesonQQ|{"scim_external_id": "me@example.com",
                        "team": "226923f0-6f15-11ee-96bd-33644427c814"}|]
            err = "Error in $: scim_external_id requires either email address or saml_id to be present"
         in mkUAuthIdTestCase "3" jsonIn err
      ],
    testGroup
      "eUserIdentityFromComponents: error cases"
      [ testCase "UserIdentityFromComponentsNoFields" $ do
          (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) =###= UserIdentityFromComponentsNoFields
          (Nothing, Nothing, Nothing, Nothing, Nothing, Just ManagedByWire) =###= UserIdentityFromComponentsNoFields
          (Nothing, Nothing, Nothing, Nothing, Just tid, Nothing) =###= UserIdentityFromComponentsNoFields
          (Nothing, Nothing, Nothing, Nothing, Nothing, Just ManagedByWire) =###= UserIdentityFromComponentsNoFields,
        testCase "UserIdentityFromComponentsNoPhoneAllowedForUAuthId" $ do
          (Nothing, Just phn1, Just (UAuthId Nothing (Just eid1) (Just ews1) tid), Nothing, Just tid, Nothing) =###= UserIdentityFromComponentsNoPhoneAllowedForUAuthId
          (Nothing, Just phn1, Nothing, Just (UserSSOId uref1), Just tid, Nothing) =###= UserIdentityFromComponentsNoPhoneAllowedForUAuthId,
        testCase "UserIdentityFromComponentsUAuthIdWithoutTeam" $ do
          -- NB: (Nothing, Nothing, Just (UAuthId Nothing (Just eid1) (Just ews1) tid), Nothing, Nothing, Nothing) is fine, we already have a team id!
          (Nothing, Nothing, Nothing, Just (UserSSOId uref1), Nothing, Nothing) =###= UserIdentityFromComponentsUAuthIdWithoutTeam,
        testCase "UserIdentityFromComponentsUAuthIdTeamMismatch" $ do
          (Nothing, Nothing, Just (UAuthId Nothing (Just eid1) (Just ews1) tid), Nothing, Just tid2, Nothing) =###= UserIdentityFromComponentsUAuthIdTeamMismatch
      ],
    testGroup
      "parse Identity: UAuthIdentity"
      $ flip fmap [Nothing, Just email1, Just email2]
      $ \mbBrigEmail ->
        testGroup ("brig email: " <> show mbBrigEmail) $
          [ -- {eid, email, team}
            let jsonIn =
                  [ ( "uauth_id",
                      [aesonQQ|{"scim_external_id": "me@example.com",
                                "email": {"email": "me@example.com", "source": "scim_external_id"},
                                "team": "226923f0-6f15-11ee-96bd-33644427c814"}|]
                    )
                  ]
                haskellIn = Right uaid
                jsonOut = jsonIn <> [("sso_id", [aesonQQ|{"scim_external_id": "me@example.com"}|])]
                uaid = UAuthId Nothing (Just eid1) (Just ews1) tid
             in mkUAuthIdentityTestCase "4" jsonIn haskellIn jsonOut mbBrigEmail,
            let jsonIn =
                  [ ( "uauth_id",
                      [aesonQQ|{"scim_external_id": "nick",
                                "email": {"email": "other@example.com", "source": "scim_emails"},
                                "team": "226923f0-6f15-11ee-96bd-33644427c814"}|]
                    )
                  ]
                haskellIn = Right uaid
                jsonOut = jsonIn <> [("sso_id", [aesonQQ|{"scim_external_id": "nick"}|])]
                uaid = UAuthId Nothing (Just eid3) (Just ews3) tid
             in mkUAuthIdentityTestCase "4.1" jsonIn haskellIn jsonOut mbBrigEmail,
            -- {saml, team}
            let jsonIn =
                  [ ( "uauth_id",
                      [aesonQQ|{"saml_id": {
                                    "subject": "<NameID xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" Format=\"urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">me@example.com</NameID>",
                                    "tenant": "<Issuer xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">http://example.com/wef</Issuer>"
                                },
                                "team": "226923f0-6f15-11ee-96bd-33644427c814"}|]
                    )
                  ]
                haskellIn = Right uaid
                jsonOut =
                  jsonIn
                    <> [ ( "sso_id",
                           [aesonQQ|{"subject": "<NameID xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" Format=\"urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">me@example.com</NameID>",
                                     "tenant": "<Issuer xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">http://example.com/wef</Issuer>"
                                    }|]
                         )
                       ]
                uaid = UAuthId (Just uref1) Nothing Nothing tid
             in mkUAuthIdentityTestCase "5" jsonIn haskellIn jsonOut mbBrigEmail,
            -- {saml, email, team}
            let jsonIn =
                  [ ( "uauth_id",
                      [aesonQQ|{"saml_id": {
                                    "subject": "<NameID xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" Format=\"urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">me@example.com</NameID>",
                                    "tenant": "<Issuer xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">http://example.com/wef</Issuer>"
                                },
                                "email": {"email": "me@example.com", "source": "scim_external_id"},
                                "team": "226923f0-6f15-11ee-96bd-33644427c814"}|]
                    )
                  ]
                haskellIn = Right uaid
                jsonOut =
                  jsonIn
                    <> [ ( "sso_id",
                           [aesonQQ|{"subject": "<NameID xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" Format=\"urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">me@example.com</NameID>",
                                     "tenant": "<Issuer xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">http://example.com/wef</Issuer>"
                                    }|]
                         )
                       ]
                uaid = UAuthId (Just uref1) Nothing (Just ews1) tid
             in mkUAuthIdentityTestCase "6" jsonIn haskellIn jsonOut mbBrigEmail,
            -- {saml, eid, team}
            let jsonIn =
                  [ ( "uauth_id",
                      [aesonQQ|{"scim_external_id": "<NameID xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" Format=\"urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">nick</NameID>",
                                "saml_id": {
                                    "subject": "<NameID xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" Format=\"urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">nick</NameID>",
                                    "tenant": "<Issuer xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">http://example.com/wef</Issuer>"
                                },
                                "email": {"email": "other@example.com", "source": "scim_emails"},
                                "team": "226923f0-6f15-11ee-96bd-33644427c814"}|]
                    )
                  ]
                haskellIn = Right uaid
                jsonOut =
                  jsonIn
                    <> [ ( "sso_id",
                           [aesonQQ|{"subject": "<NameID xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" Format=\"urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">nick</NameID>",
                                     "tenant": "<Issuer xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">http://example.com/wef</Issuer>"                                }|]
                         )
                       ]
                uaid = UAuthId (Just uref3) (Just "<NameID xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" Format=\"urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">nick</NameID>") (Just ews3) tid
             in mkUAuthIdentityTestCase "7" jsonIn haskellIn jsonOut mbBrigEmail,
            -- {saml, eid, email, team}
            let jsonIn =
                  [ ( "uauth_id",
                      [aesonQQ|{
                                   "email": {
                                       "email": "me@example.com",
                                       "source": "scim_external_id"
                                   },
                                   "saml_id": {
                                       "subject": "<NameID xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" Format=\"urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">me@example.com</NameID>",
                                       "tenant": "<Issuer xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">http://example.com/wef</Issuer>"
                                   },
                                   "scim_external_id": "me@example.com",
                                   "team": "226923f0-6f15-11ee-96bd-33644427c814"
                               }|]
                    )
                  ]
                    <> [("email", toJSON e) | e <- maybeToList mbBrigEmail]
                haskellIn = Right (UAuthId (Just uref1) (Just eid1) (Just ews1) tid)
                jsonOut =
                  jsonIn
                    <> [ ( "sso_id",
                           [aesonQQ|{
                                        "subject": "<NameID xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" Format=\"urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">me@example.com</NameID>",
                                        "tenant": "<Issuer xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">http://example.com/wef</Issuer>"
                                    }|]
                         )
                       ]
             in mkUAuthIdentityTestCase "8" jsonIn haskellIn jsonOut mbBrigEmail,
            testCase "..." $
              error "ok, what else?"
          ]
  ]
  where
    -- render jsonIn into a UAuthId parser error.
    --
    -- msg: for associating test reports with source code of test case
    -- mbBrigEmail: email address from `brig.user.email`; doesn't may or may not match email field in uauthid
    mkUAuthIdTestCase :: String -> Value -> String -> TestTree
    mkUAuthIdTestCase msg val err =
      testCase ("mkUAuthIdTestCase[" <> msg <> "]") $
        assertEqual "" (Left err) (Aeson.parseEither (schemaIn (schema @PartialUAuthId)) val)

    -- render jsonIn into a UserIdentity value, and back to its components.
    --
    -- msg: for associating test reports with source code of test case
    -- mbBrigEmail: email address from `brig.user.email`; doesn't may or may not match email field in uauthid
    mkUAuthIdentityTestCase :: String -> [Aeson.Pair] -> Either String PartialUAuthId -> [Aeson.Pair] -> Maybe Email -> TestTree
    mkUAuthIdentityTestCase msg jsonIn_ haskellIn_ jsonOut_ mbBrigEmail =
      let emailComp = [("email", String $ cs (fromEmail e)) | e <- maybeToList mbBrigEmail]
          jsonIn = jsonIn_ <> emailComp
          jsonOut = jsonOut_ <> emailComp
          haskellIn = (`UAuthIdentity` mbBrigEmail) <$> haskellIn_
          (=##=) uid comps = assertEqual "=##=" (eUserIdentityToComponents (Right uid)) comps
       in testGroup msg $
            [ testCase "in" $
                jsonIn
                  =#= (Just <$> haskellIn)
            ]
              <> ( either
                     (const [])
                     (\hsk -> [testCase "out" $ hsk =##= componentsFromJSON jsonOut])
                     haskellIn
                 )

    componentsFromJSON :: HasCallStack => [Aeson.Pair] -> UserIdentityComponents "team_id"
    componentsFromJSON obj = foldr go (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) obj
      where
        go :: Aeson.Pair -> UserIdentityComponents "team_id" -> UserIdentityComponents "team_id"
        go ("email", v) comps = upd comps _1 (Aeson.fromJSON v)
        go ("phone", v) comps = upd comps _2 (Aeson.fromJSON v)
        go ("uauth_id", v) comps = upd comps _3 (Aeson.fromJSON v)
        go ("sso_id", v) comps = upd comps _4 (Aeson.fromJSON v)
        go ("team_id", v) comps = upd comps _5 (Aeson.fromJSON v)
        go ("managed_by", v) comps = upd comps _6 (Aeson.fromJSON v)
        go bad _ = error $ unlines ["go", show obj, show bad]

        upd :: a -> Lens' a (Maybe b) -> Result b -> a
        upd comps lens (Aeson.Success v) = comps & lens ?~ v
        upd _ _ (Aeson.Error err) = error $ unlines ["upd", show obj, err]

    (=#=) :: HasCallStack => [Aeson.Pair] -> Either String (Maybe (UserIdentity "team_id")) -> Assertion
    (=#=) (object -> Object obj) uid = assertEqual "=#=" uid (Aeson.parseEither (schemaIn (maybeUserIdentityObjectSchema @"team_id")) obj)
    (=#=) _ _ = error $ "=#=: impossible"

    (=###=) :: HasCallStack => (UserIdentityComponents "team_id") -> UserIdentityFromComponentsParseErrors -> Assertion
    (=###=) comps err = assertEqual "=###=" (Left err) (eUserIdentityFromComponents comps)

    email1 = Email "me" "example.com"
    email2 = Email "other" "example.com"

    ews1 = EmailWithSource email1 EmailFromScimExternalIdField
    ews3 = EmailWithSource email2 EmailFromScimEmailsField

    eid1 = fromEmail email1
    eid3 = "nick" :: Text

    uref1 = mkBasicSampleUref "http://example.com/wef" eid1
    uref3 = mkBasicSampleUref "http://example.com/wef" "nick"

    phn1 = Phone "+123456789"

    tid = read "226923f0-6f15-11ee-96bd-33644427c814"
    tid2 = read "8298c71e-855c-11ee-9ff6-5f1a496da735"
