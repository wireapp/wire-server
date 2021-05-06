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
testObject_InvitationList_team_1 = InvitationList {ilInvitations = [Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T15:46:08.115Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\ETX.\f(\1022223\1003860je\152350#P\"\8941\SI\1047232ra'\27662X\1105967\DLE&\ENQD3=\aN\1005374\175509\&8\1076912\1000749`\1093516RyPH\DELW4VI\EOT\1000663a&H(\136759\132380=\ETB\66574\\\US\1038783~\1021888=Wt\SUBa\174024/;\54470ueBu9\1026388\1113887\&1,\1088919\134291j\n\1000125\&9\99773\DC3.\150466pE\157594\a\t\SUB>X."}), inInviteePhone = Just (Phone {fromPhone = "+859863587212"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T04:19:51.650Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\1005299\ENQ=\nt$o\1088201Xj1e8<v[O:\FS\FS\994027\SO"}), inInviteePhone = Just (Phone {fromPhone = "+624716714"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T03:51:55.992Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "xj\1015157\1090968\20715\ETX\1011265u[t\ACKr\16295=BV|j'\21967\&2_q \1094643\ETX~\150588\36052\ESCE/\1008452!\t\184554t\67317\FSDw\GS\166497\ETB\DC2;Trv\1047504=\50088'\DLE\"\150204)\ETB`iI\n\EM\DELM\1082643\28415\1027274X_8cx"}), inInviteePhone = Just (Phone {fromPhone = "+4051238302"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T03:47:47.022Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "RS]\DEL\166807\&5:\DC2K\92350:,,:V\1077998L8uB `3@\182745P5\ESC\36070a\CANl\DC3l-\NAKO\DLEy\RS-V\ESC)?_cD\190686_0\1113102kn\169851\42858\nN8#M\143982\\azc\NULYx\r\FSu9\1085899[\a\2472\GS1V\SOl\1004392:\25315\ACK\98263)p/]\1093733\a\EOTdi\ACK\14211\36210G\bN\n\126085\NAKC"}), inInviteePhone = Nothing},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T21:47:12.354Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "f8\ETX=`5I\1035668;L)\948.Gr\34486&\a2Y\\\b\1054760\1006282\48224\&4l^l\167094\ACK\1051380h\DC3}*#\DC3;\169101P&6\1080667\1027951BU~y\44314Vr|F@\1083043\fXz}.n\1109004\vuV/\GS\1027588`\DC4\1009054;\1113342\1101063Wbd\1062506\65445/oR"}), inInviteePhone = Just (Phone {fromPhone = "+14880439"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T01:05:44.852Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Nothing, inInviteePhone = Just (Phone {fromPhone = "+31440041"})}], ilHasMore = False}
testObject_InvitationList_team_2 :: InvitationList
testObject_InvitationList_team_2 = InvitationList {ilInvitations = [], ilHasMore = False}
testObject_InvitationList_team_3 :: InvitationList
testObject_InvitationList_team_3 = InvitationList {ilInvitations = [Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-08T15:24:05.802Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), inInviteeEmail = Email {emailLocal = "M", emailDomain = "\142426"}, inInviteeName = Just (Name {fromName = "b\DC1s\ESCx\49828\178752auWo2+\43154\DEL\GS\FSyQ\r\134595~"}), inInviteePhone = Just (Phone {fromPhone = "+349213512555609"})}], ilHasMore = True}
testObject_InvitationList_team_4 :: InvitationList
testObject_InvitationList_team_4 = InvitationList {ilInvitations = [Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T19:15:29.503Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\SUBDb\DLE\22305R2\a\NUL6)tK;'0\1108623\1047465/I~\ESCD=N\"xE\142382*W\vp?\1034848\128597\&3Qmf.\"\ESCH\1100217`_\994772\189197F\57621b\1103376Pg\63884\1020982Jf\99733#y\12970\NAK\DC4\NULb\1014175n!z\DC1M\184849Q!\SIRf@\CAN`\EM\RS2\1112239uH\181504\DC1dV\SOH\DC2I\ACK\1048326K &]}]"}), inInviteePhone = Just (Phone {fromPhone = "+25302858111"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T18:34:58.535Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "4 oa\999617\1073834>\nXc\147633%kN(^\1083865\147080+3G;\135307x3\24305G\1095412\29456(m\EOT\EOTiZ;g\NAK\138193jF`p\v\DELFIx"}), inInviteePhone = Just (Phone {fromPhone = "+06484479742092"})}], ilHasMore = True}
testObject_InvitationList_team_5 :: InvitationList
testObject_InvitationList_team_5 = InvitationList {ilInvitations = [], ilHasMore = False}
