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
testObject_Invitation_team_1 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-06T23:21:39.501Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000002"))), inInviteeEmail = Email {emailLocal = "", emailDomain = "\SOR\15610"}, inInviteeName = Just (Name {fromName = "G*\1058844Z\STX%c\DC3z/\tT\f\t\DC1)mW\EOT\SYN"}), inInviteePhone = Just (Phone {fromPhone = "+310708370268"})}
testObject_Invitation_team_2 :: Invitation
testObject_Invitation_team_2 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-07T17:25:54.873Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000001"))), inInviteeEmail = Email {emailLocal = "X\1039005", emailDomain = "\1033721\60383d"}, inInviteeName = Just (Name {fromName = "N!\169675\38654P*5R\ESC#<U\27324\n$6%\fn\SUB\tFi\DLEt\147685[A\127185\1103724\ESC\\4\ETB\EM9*Y?k\984965\155368>\1070882\EOT\\\7670\US@T\ETB\NUL\DLE=jcu\988905"}), inInviteePhone = Just (Phone {fromPhone = "+755218294624800"})}
testObject_Invitation_team_3 :: Invitation
testObject_Invitation_team_3 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000001"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-06T10:42:24.753Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "EW", emailDomain = ""}, inInviteeName = Just (Name {fromName = "I\DEL\FS\ETBu\b\\g\CANP&\1094015I;\SO\NUL5!g\1043524t\1107555T\155049\DC2\a\60239$3\STX@@\1080594;\20527\RS\NAK"}), inInviteePhone = Just (Phone {fromPhone = "+26946824"})}
testObject_Invitation_team_4 :: Invitation
testObject_Invitation_team_4 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-11T00:54:48.193Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000001"))), inInviteeEmail = Email {emailLocal = "\1048940#", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\163553\t\1095726]x98\NULo\1057714\6836\DC3\1110948B4~Zl{\986510\92232IE\1035184\1005877\&9"}), inInviteePhone = Just (Phone {fromPhone = "+418992008891915"})}
testObject_Invitation_team_5 :: Invitation
testObject_Invitation_team_5 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T10:11:12.118Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000002"))), inInviteeEmail = Email {emailLocal = "", emailDomain = "d"}, inInviteeName = Just (Name {fromName = "\1017013+e@X-18\131635\156046/\174088\1002901A\f\57728hvs\b\997505q/{\DLE\ESCsW(\r6\SUB\US\995037\58807se\1045066I\52219\rLBP\SOH\42235u2\ETX[\187388\USrw"}), inInviteePhone = Just (Phone {fromPhone = "+31825386897271"})}
