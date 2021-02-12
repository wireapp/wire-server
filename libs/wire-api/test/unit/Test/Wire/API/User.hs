{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

import Data.Aeson (FromJSON (parseJSON), object)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Value, parseEither)
import Data.Domain
import Data.Id
import Data.Qualified
import Data.String.Conversions (cs)
import qualified Data.UUID.V4 as UUID
import Imports
import SAML2.WebSSO.Types (Issuer (..), UserRef (..), unspecifiedNameID)
import qualified SAML2.WebSSO.Types as SAML
import Test.Tasty
import Test.Tasty.HUnit
import URI.ByteString (parseURI, strictURIParserOptions)
import URI.ByteString.QQ (uri)
import Wire.API.User

tests :: TestTree
tests = testGroup "User (types vs. aeson)" $ unitTests

unitTests :: [TestTree]
unitTests = parseIdentityTests ++ jsonNullTests ++ [authIdTests, legacyAuthIdTests] ++ parseNewUser

jsonNullTests :: [TestTree]
jsonNullTests = [testGroup "JSON null" [testCase "userProfile" $ testUserProfile]]

testUserProfile :: Assertion
testUserProfile = do
  uid <- Id <$> UUID.nextRandom
  let domain = Domain "example.com"
  let colour = ColourId 0
  let userProfile = UserProfile (Qualified uid domain) (Name "name") (Pict []) [] colour False Nothing Nothing Nothing Nothing Nothing Nothing
  let profileJSONAsText = show $ Aeson.encode userProfile
  let msg = "toJSON encoding must not convert Nothing to null, but instead omit those json fields for backwards compatibility. UserProfileJSON:" <> profileJSONAsText
  assertBool msg (not $ "null" `isInfixOf` profileJSONAsText)

parseIdentityTests :: [TestTree]
parseIdentityTests =
  [ let (=#=) :: Either String UserIdentity -> Value -> Assertion
        (=#=) target val = assertEqual "=#=" target (parseEither parseJSON val)
     in testGroup
          "UserIdentity FromJSON"
          [ testCase "FullIdentity" $
              Right (FullIdentity hemail hphone) =#= object [email, phone],
            testCase "EmailIdentity" $
              Right (EmailIdentity hemail) =#= object [email],
            testCase "PhoneIdentity" $
              Right (PhoneIdentity hphone) =#= object [phone],
            testCase "AuthIdentity" $ do
              Right (SparAuthIdentity hAuthId Nothing Nothing) =#= object [authId]
              Right (SparAuthIdentity hAuthId Nothing (Just hphone)) =#= object [authId, phone]
              Right (SparAuthIdentity hAuthId (Just hemail) Nothing) =#= object [authId, email]
              Right (SparAuthIdentity hAuthId (Just hemail) (Just hphone)) =#= object [authId, email, phone],
            testCase "Bad phone" $
              Left "Error in $.phone: Invalid phone number. Expected E.164 format." =#= object [badphone],
            testCase "Bad email" $
              Left "Error in $.email: Invalid email. Expected '<local>@<domain>'." =#= object [bademail],
            testCase "Bad email" $
              Left "Error in $.email: Invalid email. Expected '<local>@<domain>'." =#= object [bademail, authId]
          ]
  ]
  where
    hemail = Email "me" "example.com"
    email = ("email", "me@example.com")
    bademail = ("email", "justme")
    hphone = Phone "+493012345678"
    phone = ("phone", "+493012345678")
    badphone = ("phone", "__@@")
    hAuthId = AuthSAML $ exampleUserRef 1 "1"
    authId = ("auth_id", Aeson.toJSON hAuthId)

parseNewUser :: [TestTree]
parseNewUser =
  [ testGroup
      "NewUser FromJSON"
      [ testCase "object with valid email parses" $
          check True (object [name, email]),
        testCase "object with invalid email fails" $
          check False (object [name, bademail]),
        testCase "object with valid phone number parses" $
          check True (object [name, phone]),
        testCase "object with invalid phone number fails" $
          check False (object [name, badphone]),
        testCase "object with valid auth_id parses" $
          check True (object [name, teamId, authId]),
        testCase "object with bad auth_id fails" $
          check False (object [name, teamId, badAuthId])
      ]
  ]
  where
    check shouldSucceed value =
      assertBool "" $
        (if shouldSucceed then isRight else isLeft) $
          parseEither (parseJSON @NewUser) value

    name = ("name", "Ada")
    email = ("email", "me@example.com")
    bademail = ("email", "justme")
    phone = ("phone", "+493012345678")
    badphone = ("phone", "__@@")
    hAuthId = AuthSAML $ exampleUserRef 1 "1"
    authId = ("auth_id", Aeson.toJSON hAuthId)
    badAuthId = ("auth_id", "garbage")
    teamId = ("team_id", "487b507e-31bc-4d1d-b8cf-af632ea2e6d8")

exampleUserRef :: Int -> Text -> UserRef
exampleUserRef domainSuffix name =
  let uri' = either (error "exampleUserRef") id $ parseURI strictURIParserOptions (cs ("https://www.example" <> show domainSuffix <> ".com"))
   in UserRef (Issuer uri') (unspecifiedNameID name)

authIdTests :: TestTree
authIdTests = testCase "parse AuthId" $ do
  let samples :: [(LByteString, AuthId)]
      samples =
        [ ( "{\"type\":\"saml\",\"subject\":\"<NameID xmlns:samlp=\\\"urn:oasis:names:tc:SAML:2.0:protocol\\\" xmlns:samla=\\\"urn:oasis:names:tc:SAML:2.0:assertion\\\" xmlns:samlm=\\\"urn:oasis:names:tc:SAML:2.0:metadata\\\" xmlns:ds=\\\"http://www.w3.org/2000/09/xmldsig#\\\" Format=\\\"urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress\\\" xmlns=\\\"urn:oasis:names:tc:SAML:2.0:assertion\\\">me@example.com</NameID>\",\"tenant\":\"https://sso.example.com/2b6d50b8-7c31-11eb-b2ef-1fb8ec297153\"}",
            AuthSAML
              ( SAML.UserRef
                  (SAML.Issuer [uri|https://sso.example.com/2b6d50b8-7c31-11eb-b2ef-1fb8ec297153|])
                  (fromRight (error "impossible") (SAML.emailNameID "me@example.com"))
              )
          ),
          ( "{\"type\":\"scim\",\"team\":\"b6e09060-7b73-11eb-af7f-9b48ef85f610\",\"external_id\":\"user@example.com\",\"email\":\"user@example.com\",\"email_source\":\"EmailFromExternalIdField\"}",
            AuthSCIM
              ( ScimDetails
                  (ExternalId (read "b6e09060-7b73-11eb-af7f-9b48ef85f610") "user@example.com")
                  (EmailWithSource (fromJust $ parseEmail "user@example.com") EmailFromExternalIdField)
              )
          )
        ]

  forM_ samples $ \(input, want) -> do
    let have = Aeson.eitherDecode input
    assertEqual (cs input) (Right want) have

legacyAuthIdTests :: TestTree
legacyAuthIdTests = testCase "parse legacy UserSSOId values as LegacyAuthId" $ do
  let tid :: TeamId
      tid = read "b6e09060-7b73-11eb-af7f-9b48ef85f610"

      samples :: [(LByteString, AuthId)]
      samples =
        [ ( -- Wire.API.User.UserSSOId "https://sso.example.com/2b6d50b8-7c31-11eb-b2ef-1fb8ec297153" "<NameID xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" Format=\"urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">me@example.com</NameID>"
            "{\"subject\":\"<NameID xmlns:samlp=\\\"urn:oasis:names:tc:SAML:2.0:protocol\\\" xmlns:samla=\\\"urn:oasis:names:tc:SAML:2.0:assertion\\\" xmlns:samlm=\\\"urn:oasis:names:tc:SAML:2.0:metadata\\\" xmlns:ds=\\\"http://www.w3.org/2000/09/xmldsig#\\\" Format=\\\"urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress\\\" xmlns=\\\"urn:oasis:names:tc:SAML:2.0:assertion\\\">me@example.com</NameID>\",\"tenant\":\"https://sso.example.com/2b6d50b8-7c31-11eb-b2ef-1fb8ec297153\"}",
            AuthSAML
              ( SAML.UserRef
                  (SAML.Issuer [uri|https://sso.example.com/2b6d50b8-7c31-11eb-b2ef-1fb8ec297153|])
                  (fromRight (error "impossible") (SAML.emailNameID "me@example.com"))
              )
          ),
          ( -- Wire.API.User.UserSSOId "https://sso.example.com/2b6d50b8-7c31-11eb-b2ef-1fb8ec297153" "<NameID xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">ab70db04-7c31-11eb-b1d4-b7a41c3589b1</NameID>"
            "{\"subject\":\"<NameID xmlns:samlp=\\\"urn:oasis:names:tc:SAML:2.0:protocol\\\" xmlns:samla=\\\"urn:oasis:names:tc:SAML:2.0:assertion\\\" xmlns:samlm=\\\"urn:oasis:names:tc:SAML:2.0:metadata\\\" xmlns:ds=\\\"http://www.w3.org/2000/09/xmldsig#\\\" xmlns=\\\"urn:oasis:names:tc:SAML:2.0:assertion\\\">ab70db04-7c31-11eb-b1d4-b7a41c3589b1</NameID>\",\"tenant\":\"https://sso.example.com/2b6d50b8-7c31-11eb-b2ef-1fb8ec297153\"}",
            AuthSAML
              ( SAML.UserRef
                  (SAML.Issuer [uri|https://sso.example.com/2b6d50b8-7c31-11eb-b2ef-1fb8ec297153|])
                  (SAML.unspecifiedNameID "ab70db04-7c31-11eb-b1d4-b7a41c3589b1")
              )
          ),
          ( -- Wire.API.User.Identity.UserScimExternalId "user@example.com"
            "{\"scim_external_id\":\"user@example.com\"}",
            AuthSCIM
              ( ScimDetails
                  (ExternalId tid "user@example.com")
                  (EmailWithSource (fromJust $ parseEmail "user@example.com") EmailFromExternalIdField)
              )
          )
        ]

  forM_ samples $ \(input, want) -> do
    let have = (($ tid) . fromLegacyAuthId) <$> Aeson.eitherDecode input
    assertEqual (cs input) (Right want) have
