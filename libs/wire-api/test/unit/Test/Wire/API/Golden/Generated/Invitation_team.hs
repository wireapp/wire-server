{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Invitation_team where

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
testObject_Invitation_team_1 :: Invitation
testObject_Invitation_team_1 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-08T23:37:29.019Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "", emailDomain = "-\\\GS"}, inInviteeName = Just (Name {fromName = "u\ENQ\aD\ESC\SI\73971\1054567U:)\NULp\180566\SO\RS\118995\&2\1059536\1036798\41776\134855i?\DC2}?\1054233#\RS\tA5\"4i1?\a\STX\ETX\83482\1029179\b\SOH0\FSc\4162\DC3\v\FS+=O\1008473s\1025975\120787^Wq\EMr\NUL\6807&\DC3D3?\1070068|\991530\1012120\145042\ESC\6946\1040160|\179118<<\150339b\ACK7EF\DC35\133789\bS\SOH\998202\r\DC2g\132130w\149135"}), inInviteePhone = Just (Phone {fromPhone = "+5475453685"})}
testObject_Invitation_team_2 :: Invitation
testObject_Invitation_team_2 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-11T01:26:00.101Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000002"))), inInviteeEmail = Email {emailLocal = "", emailDomain = "M\v"}, inInviteeName = Just (Name {fromName = "\DC4y2q\r"}), inInviteePhone = Just (Phone {fromPhone = "+05172453649022"})}
testObject_Invitation_team_3 :: Invitation
testObject_Invitation_team_3 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T20:04:18.440Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000"))), inInviteeEmail = Email {emailLocal = "", emailDomain = "\1070029"}, inInviteeName = Nothing, inInviteePhone = Nothing}
testObject_Invitation_team_4 :: Invitation
testObject_Invitation_team_4 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000001"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-08T15:25:04.996Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), inInviteeEmail = Email {emailLocal = "|i\1049663", emailDomain = "\EOT7"}, inInviteeName = Just (Name {fromName = "jWxa\173281B\ESC\18206\&9!\1072389,4"}), inInviteePhone = Just (Phone {fromPhone = "+7562290827"})}
testObject_Invitation_team_5 :: Invitation
testObject_Invitation_team_5 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-06T13:42:13.345Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000000"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "g#MxQ0G\1046200\&4vS_ e.)`\59800-<I"}), inInviteePhone = Just (Phone {fromPhone = "+0512741184256"})}
