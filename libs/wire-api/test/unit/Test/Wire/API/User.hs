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

import Control.Lens ((?~), _1, _2, _3, _4, _5, _6)
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types qualified as Aeson
import Data.Domain
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (UserLegalHoldNoConsent))
import Data.Qualified
import Data.Schema (schemaIn)
import Data.UUID.V4 qualified as UUID
import Imports
import Servant.API (parseUrlPiece)
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
      "parseIdentity (simple cases)"
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
      "parseIdentity (UAuthId)"
      $ flip fmap [Nothing, Just email1, Just email2]
      $ \mbBrigEmail ->
        testGroup ("brig email: " <> show mbBrigEmail) $
          [ let jsonIn = [("uauth_id", [aesonQQ|{}|])]
                haskellIn = Left "Error in $['uauth_id']: key \"team\" not found"
                jsonOut = error "impossible"
             in mkUAuthIdTestCase "1.1" jsonIn haskellIn jsonOut mbBrigEmail,
            let jsonIn = [("uauth_id", [aesonQQ|{"team": "7e869040-7fab-11ee-9dff-0bb0f984d9c8"}|])]
                haskellIn = Left "" -- Right (Just (UAuthIdentity (UAuthId {uaSamlId = Nothing, uaScimExternalId = Nothing, uaEmail = Nothing, uaTeamId = 7e869040-7fab-11ee-9dff-0bb0f984d9c8}) Nothing))
                jsonOut = error "impossible"
             in mkUAuthIdTestCase "1.2" jsonIn haskellIn jsonOut mbBrigEmail,
            let jsonIn = [("uauth_id", [aesonQQ|{}|])]
                haskellIn = Left ""
                jsonOut = error "impossible"
             in mkUAuthIdTestCase "2" jsonIn haskellIn jsonOut mbBrigEmail,
            let jsonIn = [("uauth_id", [aesonQQ|{}|])]
                haskellIn = Left ""
                jsonOut = error "impossible"
             in mkUAuthIdTestCase "3" jsonIn haskellIn jsonOut mbBrigEmail,
            let jsonIn = [("uauth_id", [aesonQQ|{}|])]
                haskellIn = Right uaid
                jsonOut = jsonIn
                uaid = UAuthId Nothing (Just eid1) (Just ews1) tid
             in mkUAuthIdTestCase "4" jsonIn haskellIn jsonOut mbBrigEmail,
            let jsonIn = [("uauth_id", [aesonQQ|{}|])]
                haskellIn = Right uaid
                jsonOut = jsonIn
                uaid = UAuthId (Just uref1) Nothing Nothing tid
             in mkUAuthIdTestCase "5" jsonIn haskellIn jsonOut mbBrigEmail,
            let jsonIn = [("uauth_id", [aesonQQ|{}|])]
                haskellIn = Right uaid
                jsonOut = jsonIn
                uaid = UAuthId (Just uref1) Nothing (Just ews1) tid
             in mkUAuthIdTestCase "6" jsonIn haskellIn jsonOut mbBrigEmail,
            let jsonIn = [("uauth_id", [aesonQQ|{}|])]
                haskellIn = Right uaid
                jsonOut = jsonIn
                uaid = UAuthId (Just uref1) (Just eid1) Nothing tid
             in mkUAuthIdTestCase "7" jsonIn haskellIn jsonOut mbBrigEmail,
            let jsonIn = [("uauth_id", [aesonQQ|{}|])]
                haskellIn = Right uaid
                jsonOut = jsonIn
                uaid = UAuthId (Just uref1) (Just eid1) (Just ews1) tid
             in mkUAuthIdTestCase "8" jsonIn haskellIn jsonOut mbBrigEmail,
            testCase "..." $
              error "ok, what else?"
          ]
  ]
  where
    -- render jsonIn into a UserIdentity value, and back to its components.
    --
    -- msg: for associating test reports with source code of test case
    -- mbBrigEmail: email address from `brig.user.email`; doesn't may or may not match email field in uauthid
    mkUAuthIdTestCase :: String -> [Aeson.Pair] -> Either String (PartialUAuthId "team_id") -> [Aeson.Pair] -> Maybe Email -> TestTree
    mkUAuthIdTestCase msg jsonIn_ haskellIn_ jsonOut mbBrigEmail =
      let jsonIn = jsonIn_ <> [("email", String $ cs (fromEmail e)) | e <- maybeToList mbBrigEmail]
          haskellIn = (`UAuthIdentity` mbBrigEmail) <$> haskellIn_
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

    componentsFromJSON :: [Aeson.Pair] -> UserIdentityComponents "team_id"
    componentsFromJSON = foldr go (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
      where
        go :: Aeson.Pair -> UserIdentityComponents "team_id" -> UserIdentityComponents "team_id"
        go ("email", Aeson.fromJSON -> (Aeson.Success v)) comps = comps & _1 ?~ v
        go ("phone", Aeson.fromJSON -> (Aeson.Success v)) comps = comps & _2 ?~ v
        go ("auth_id", Aeson.fromJSON -> (Aeson.Success v)) comps = comps & _3 ?~ v
        go ("sso_id", Aeson.fromJSON -> (Aeson.Success v)) comps = comps & _4 ?~ v
        go ("team_id", Aeson.fromJSON -> (Aeson.Success v)) comps = comps & _5 ?~ v
        go ("managed_by", Aeson.fromJSON -> (Aeson.Success v)) comps = comps & _6 ?~ v

    (=#=) :: HasCallStack => [Aeson.Pair] -> Either String (Maybe (UserIdentity "team_id")) -> Assertion
    (=#=) (object -> Object obj) uid = assertEqual "=#=" uid (Aeson.parseEither (schemaIn (maybeUserIdentityObjectSchema @"team_id")) obj)
    (=#=) _ _ = error $ "=#=: impossible"

    (=##=) :: HasCallStack => UserIdentity "team_id" -> (UserIdentityComponents "team_id") -> Assertion
    (=##=) uid comps = assertEqual "=##=" (eUserIdentityToComponents (Right uid)) comps

    (=###=) :: HasCallStack => (UserIdentityComponents "team_id") -> UserIdentityFromComponentsParseErrors -> Assertion
    (=###=) comps err = assertEqual "=###=" (eUserIdentityFromComponents comps) (Left err)

    email1 = Email "me" "example.com"
    email2 = Email "other" "example.com"

    ews1 = EmailWithSource email1 EmailFromScimExternalIdField
    ews2 = EmailWithSource email2 EmailFromScimExternalIdField
    ews3 = EmailWithSource email2 EmailFromScimEmailsField

    eid1 = fromEmail email1
    eid2 = fromEmail email2
    eid3 = "nick" :: Text

    uref1 = mkSampleUref "http://example.com/wef" eid1
    uref2 = mkSampleUref "http://example.com/wef" eid2
    uref3 = mkSampleUref "http://example.com/wef" "nick"

    Right tid = parseUrlPiece "226923f0-6f15-11ee-96bd-33644427c814"
