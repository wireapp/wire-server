{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans #-}

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

module Test.Wire.API.Golden.Manual.Identity
  ( testObject_UAuthId_1,
    testObject_UAuthId_2,
    testObject_UAuthId_3,
    testObject_UAuthId_4,
    testObject_UAuthId_5,
    testObject_UAuthId_6,
    testObject_UserIdentity_1,
    testObject_UserIdentity_2,
    testObject_UserIdentity_3,
    testObject_UserIdentity_4,
  )
where

import Data.Aeson
import Data.Id
import Data.Schema
import GHC.TypeLits
import Imports
import SAML2.WebSSO.Types qualified as SAML
import Web.HttpApiData (parseUrlPiece)
import Wire.API.User.Identity

sampleUref :: SAML.UserRef
sampleUref = mkSampleUref "http://example.com" "it"

sampleExtId :: Text
sampleExtId = "it"

sampleEmail :: EmailWithSource
sampleEmail = EmailWithSource (Email "it" "example.com") EmailFromScimEmailsField

sampleEmail2 :: Email
sampleEmail2 = Email "it2" "example.com"

samplePhone :: Phone
samplePhone = Phone "+1123123123"

sampleTeamId :: TeamId
Right sampleTeamId = parseUrlPiece "579edcd0-6f1b-11ee-b49a-e770ab99392a"

testObject_UAuthId_1 :: PartialUAuthId
testObject_UAuthId_1 = UAuthId Nothing (Just sampleExtId) (Just sampleEmail) sampleTeamId

testObject_UAuthId_2 :: PartialUAuthId
testObject_UAuthId_2 = UAuthId (Just sampleUref) Nothing Nothing sampleTeamId

testObject_UAuthId_3 :: PartialUAuthId
testObject_UAuthId_3 = UAuthId (Just sampleUref) (Just sampleExtId) Nothing sampleTeamId

testObject_UAuthId_4 :: PartialUAuthId
testObject_UAuthId_4 = UAuthId (Just sampleUref) (Just sampleExtId) (Just sampleEmail) sampleTeamId

testObject_UAuthId_5 :: ScimUAuthId
testObject_UAuthId_5 = UAuthId (Just sampleUref) (Identity sampleExtId) (Just sampleEmail) sampleTeamId

testObject_UAuthId_6 :: ScimUAuthId
testObject_UAuthId_6 = UAuthId Nothing (Identity sampleExtId) (Just sampleEmail) sampleTeamId

testObject_UserIdentity_1 :: UserIdentity "team_id"
testObject_UserIdentity_1 = EmailIdentity (ewsEmail sampleEmail)

testObject_UserIdentity_2 :: UserIdentity "team_id"
testObject_UserIdentity_2 = PhoneIdentity samplePhone

testObject_UserIdentity_3 :: UserIdentity "team_id"
testObject_UserIdentity_3 = FullIdentity (ewsEmail sampleEmail) samplePhone

testObject_UserIdentity_4 :: UserIdentity "team_id"
testObject_UserIdentity_4 = UAuthIdentity testObject_UAuthId_4 (Just sampleEmail2)

-- | This type is usually embedded in a bigger record, but this instance is handy for testing.
instance KnownSymbol tf => ToSchema (UserIdentity tf) where
  schema = Data.Schema.object "UserIdentity" (userIdentityObjectSchema @tf)

deriving via (Schema (UserIdentity tf)) instance KnownSymbol tf => ToJSON (UserIdentity tf)

deriving via (Schema (UserIdentity tf)) instance KnownSymbol tf => FromJSON (UserIdentity tf)
