{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Event_team where

import Codec.MIME.Type (Type(..))
import qualified Codec.MIME.Type as MIME
import Control.Lens ((.~))
import Data.Code
import Data.Coerce
import Data.Currency
import Data.Domain
import Data.Handle
import Data.Id
import Data.ISO3166_CountryCodes
import Data.Json.Util
import Data.List1
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty (..))
import Data.Misc
import Data.PEM
import Data.Qualified
import Data.Range (unsafeRange)
import qualified Data.Set as Set
import Data.Text.Ascii
import Data.Time (secondsToNominalDiffTime)
import Imports hiding (LT, GT)
import qualified Data.LanguageCodes
import qualified Data.UUID as UUID
import Test.Tasty (testGroup, TestTree)
import URI.ByteString
import qualified Wire.API.Call.Config as CallConfig
import qualified Wire.API.User.Profile as User.Profile
import qualified Wire.API.Team.Conversation as Team.Conversation
import qualified Wire.API.Provider as Provider
import qualified Wire.API.Provider.Bot as Provider
import qualified Wire.API.Provider.External as Provider
import qualified Wire.API.Provider.Service as Provider
import qualified Wire.API.Provider.Service.Tag as Provider
import Data.Aeson
import GHC.Exts
import Data.LegalHold
import Wire.API.Conversation.Role
import Wire.API.Event.Team
import Wire.API.Provider.Service
import Wire.API.Team
import Wire.API.Team.Conversation
import Wire.API.Team.Feature
import Wire.API.Team.Invitation
import Wire.API.Team.LegalHold
import Wire.API.Team.LegalHold.External
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.Team.Role
import Wire.API.Team.SearchVisibility
import Wire.API.User.Client.Prekey
import Wire.API.User.Identity
import Wire.API.User.Profile
testObject_Event_team_1 :: Event
testObject_Event_team_1 = (newEvent (MemberUpdate) ((Id (fromJust (UUID.fromString "00001ebb-0000-771e-0000-31b4000057b6")))) (read ("1864-04-23 05:54:30.861908017181 UTC")) & eventData .~ (Just (EdMemberUpdate (Id (fromJust (UUID.fromString "00005888-0000-3df6-0000-4a36000052a6"))) Nothing)))
testObject_Event_team_2 :: Event
testObject_Event_team_2 = (newEvent (ConvCreate) ((Id (fromJust (UUID.fromString "00006660-0000-291d-0000-48e100004307")))) (read ("1864-05-28 04:43:32.681524394236 UTC")) & eventData .~ (Just (EdConvCreate (Id (fromJust (UUID.fromString "00007909-0000-6a2b-0000-6bec00000238"))))))
testObject_Event_team_3 :: Event
testObject_Event_team_3 = (newEvent (ConvCreate) ((Id (fromJust (UUID.fromString "00003f52-0000-45be-0000-505f000016ca")))) (read ("1864-05-22 04:29:56.17837254606 UTC")) & eventData .~ (Just (EdConvCreate (Id (fromJust (UUID.fromString "000068da-0000-3fc5-0000-028900004b5a"))))))
testObject_Event_team_4 :: Event
testObject_Event_team_4 = (newEvent (MemberUpdate) ((Id (fromJust (UUID.fromString "0000435d-0000-2131-0000-1b1900000655")))) (read ("1864-05-29 02:20:54.849854114909 UTC")) & eventData .~ (Just (EdMemberUpdate (Id (fromJust (UUID.fromString "0000122d-0000-1a9a-0000-68ba00001bb1"))) Nothing)))
testObject_Event_team_5 :: Event
testObject_Event_team_5 = (newEvent (TeamCreate) ((Id (fromJust (UUID.fromString "00002b9c-0000-6799-0000-39b90000441d")))) (read ("1864-05-31 21:39:45.634928131839 UTC")) & eventData .~ (Just (EdTeamCreate (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000400000004")))) ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000002")))) ("]M\1081950") ("n") (NonBinding) & teamIconKey .~ (Nothing)))))
