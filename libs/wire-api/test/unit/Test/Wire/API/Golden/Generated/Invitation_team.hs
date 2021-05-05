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
testObject_Invitation_1 :: Invitation
testObject_Invitation_1 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T16:29:38.184Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000002"))), inInviteeEmail = Email {emailLocal = "", emailDomain = "S\b"}, inInviteeName = Nothing, inInviteePhone = Just (Phone {fromPhone = "+8866934062"})}
testObject_Invitation_2 :: Invitation
testObject_Invitation_2 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T04:43:38.607Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), inInviteeEmail = Email {emailLocal = "O\24426", emailDomain = "\EM"}, inInviteeName = Just (Name {fromName = "5\b@\159353?\1055685\1027313;\NAK&f\1009682U\f\1036568V\b2I\SUB\5553_\SOH\SUBE`Yk5\NAK\ETX\GS\38830Fu>m\1103961\1074548\v>\SOHa\1029920\DC4\NAKC4Dw\SUB 2W\146765$t\no\FSH\990292\95717"}), inInviteePhone = Just (Phone {fromPhone = "+2539037706"})}
testObject_Invitation_3 :: Invitation
testObject_Invitation_3 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000000"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-12T00:44:38.170Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "\51003", emailDomain = ""}, inInviteeName = Just (Name {fromName = "~IFyP|D\DC2\993143\SYN[\bJE\ENQ\1084993\&2-P%5qPx2\SOkV~/N\31295c\188799\997574\1013592\bKoUI\ETX\"\1066499(\34231d\1027576K5^h\r\185904=r\1088870\SUBJ\1078737\b\DELom&\FSPQ\111305\1048979\1112337w\1061282z\DC3\1057389$X\1087993@\33786pqE\DC2Z;lD/FVs\DELMa\147534re}\\\1039815<e\ESCuj4u19A\42852(l\SI"}), inInviteePhone = Just (Phone {fromPhone = "+060134351984367"})}
testObject_Invitation_4 :: Invitation
testObject_Invitation_4 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T02:57:42.828Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001"))), inInviteeEmail = Email {emailLocal = "}", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\60447\&4;(\roVx\184891w\DC4\b}f9\996953~"}), inInviteePhone = Just (Phone {fromPhone = "+32764571938"})}
testObject_Invitation_5 :: Invitation
testObject_Invitation_5 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-11T17:40:42.735Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = "O}a"}, inInviteeName = Just (Name {fromName = "\71071k\EOTLv\1010534\&9tP\SOH^w\ft\r(A;1\41913\54141\29319\ACKD\1055556a\191328\1077231B/\1087513YEH\GS\1034329g!x\1081594\1106108\DEL\NAK;\120194\63862\EM\122891c4\58778\1094700\&3,7\37188\EM\180403\DELMb/\53792w\NUL@\1085052V\1114043g=\1110068G\vz\1083624Q\ETB_4\SI\\\119817\SI\1028396"}), inInviteePhone = Just (Phone {fromPhone = "+49494620679715"})}
