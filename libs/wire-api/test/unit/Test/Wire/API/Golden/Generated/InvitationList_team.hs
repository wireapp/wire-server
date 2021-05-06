{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.InvitationList_team where

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
testObject_InvitationList_team_1 :: InvitationList
testObject_InvitationList_team_1 = InvitationList {ilInvitations = [Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T09:32:57.395Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\ESCV\156027\&8)\SI>9\1095939\1061409N\EOTed9a\tL.n\DEL+\150594F'E6H=\1098973r\ACKst\SOf\983283\29153\1086042\DLED}3|\EM\RS\ETB@\ENQ!G\7731\fl;\DC2H\1009303H\bpT\1062738dM\997828%\169142\DC1\SOH"}), inInviteePhone = Just (Phone {fromPhone = "+662971767756645"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T02:28:49.560Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\151958)\1112967\1091576Q0\nV?Qi4\1081407\1031152"}), inInviteePhone = Just (Phone {fromPhone = "+73045826196"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T20:53:16.375Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\1062619X%\30615.\SYN&$\US\ESC5q.Sz ]?OC67qV\1073726\1076747\1045509o\1006962kQaA\DEL\50865aF\"\1098282A'\53217#plM\SUB\54292\ESC/\DC4\51621s\1053975'C\SI\DC4\142686\51183\EMuI,\1048295=\96785\b}\137950TR5\DEL\1104502\154444|=\156661jd\bL\176669'\CAN^\RS`>=\1050642\154132\EOT\1036291\SIkH\168041_"}), inInviteePhone = Just (Phone {fromPhone = "+361467732110598"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T18:05:29.946Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Nothing, inInviteePhone = Just (Phone {fromPhone = "+1470808178"})}], ilHasMore = False}
testObject_InvitationList_team_2 :: InvitationList
testObject_InvitationList_team_2 = InvitationList {ilInvitations = [], ilHasMore = True}
testObject_InvitationList_team_3 :: InvitationList
testObject_InvitationList_team_3 = InvitationList {ilInvitations = [Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T06:19:49.272Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\32493]\1001124\1062373.\ENQI5"}), inInviteePhone = Just (Phone {fromPhone = "+99531351517459"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T02:19:19.709Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "9\SO\1016949%\145130lZ2%9s\33109\&4\1042275\EOT^^\1099513$\59645\997690K\1101886\RS\54014cz\DEL\1005693n\151054\1035077U/2\1105614\SIh\1056132`dJ"}), inInviteePhone = Just (Phone {fromPhone = "+2683502862"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T03:22:10.014Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Nothing, inInviteePhone = Just (Phone {fromPhone = "+410298826604111"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T01:36:09.413Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Nothing, inInviteePhone = Just (Phone {fromPhone = "+320877304848967"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T22:22:53.132Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Nothing, inInviteePhone = Nothing}], ilHasMore = True}
testObject_InvitationList_team_4 :: InvitationList
testObject_InvitationList_team_4 = InvitationList {ilInvitations = [], ilHasMore = False}
testObject_InvitationList_team_5 :: InvitationList
testObject_InvitationList_team_5 = InvitationList {ilInvitations = [Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T06:50:25.211Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "Qr(KY\997992z\DC1Y\158179Z\DC1$/J\1000535%"}), inInviteePhone = Just (Phone {fromPhone = "+716532274"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T02:33:52.339Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "io\1014110b\SYN\ta\SIU)&hpY0\156010k\1104562\94416\&54-8\DLE"}), inInviteePhone = Just (Phone {fromPhone = "+509315731078988"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T22:09:15.056Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = ")\138073\27981\92483gD:v\1069638o\1022274\1001056\&7\68209\STX\DC4\155677sC\166660\SO}\n\nV^\ENQ\DLEj\63308s(G\179340N-\995032\SOH\NULpX\tv\1037042=\1042618M\DC2MOG]D)Mx\1059426uu2lf\2970xtg\993987.thh\FS\SYN\32828\&6(d_\FSer\1008849\&2fj\DLE\1061893\EM[\120186G\19726\37725\20826\989095\NUL\159149<\1026241\100276\EMe "}), inInviteePhone = Nothing}], ilHasMore = False}
