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
testObject_Invitation_team_1 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-08T04:27:55.819Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000001"))), inInviteeEmail = Email {emailLocal = "4\1017895\\", emailDomain = "\1007042\1070568"}, inInviteeName = Just (Name {fromName = "\1074457a;\FS4B\1000711\51460$\988482\a&\SUB,PxB\68625\ETB\ESCZwr\STXf;c\ENQ\986245\983250#\a"}), inInviteePhone = Just (Phone {fromPhone = "+071351957"})}
testObject_Invitation_team_2 :: Invitation
testObject_Invitation_team_2 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-12T00:30:58.643Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))), inInviteeEmail = Email {emailLocal = "`\DLE", emailDomain = "t"}, inInviteeName = Just (Name {fromName = ":]\SYN\EOT\ETB\161691-\1111932B\190379'\DLE6N\ACK/\tytmYV=<y\\\US\DLE;\22856\&82\185191\1014462Uy\983662\SOas\176677Z4o\14254Z \58630bp\ETXQ!\1041539\FS\DC1\1048475yF\ACK\1064152a~\SYNdf\61079>H\NUL<\135488\1112882\&4\v\v/\992540\1076456\f8K\1041683'8\1004076_]\145681\7117X\1018696\60782l\26714f\DC2+j\ESCuqQ&@\NUL\SUB\148504"}), inInviteePhone = Just (Phone {fromPhone = "+454057870"})}
testObject_Invitation_team_3 :: Invitation
testObject_Invitation_team_3 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000002"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-06T14:37:38.588Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000"))), inInviteeEmail = Email {emailLocal = "\1012625\&8", emailDomain = "{"}, inInviteeName = Just (Name {fromName = "6I\FS\1001778\"9\1017162\DC2?\72856\ahuI\96587\47242A8s\1096540KwA\53962\51484[\21800f@^}\997339E^z?~n\"\144180&\DLE8\SI\159427g\1035330$1\34763\EM\DC2\1017083\1113784\ENQ\31729C<UB\169522*\1080207\1015889,9g\ACK\999335fiM_s3\SYN\DC4\DC4\985161\DC4\1108228N"}), inInviteePhone = Just (Phone {fromPhone = "+674306869686723"})}
testObject_Invitation_team_4 :: Invitation
testObject_Invitation_team_4 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-08T09:24:38.953Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000002"))), inInviteeEmail = Email {emailLocal = "\166237N", emailDomain = "6f"}, inInviteeName = Just (Name {fromName = "6\a\GSK\r\999998\37941\189091\995975y\989989{{SW\DC2\GS\NUL>)^\1073505\bJ^\ETXF9tALf\btU5W\vEL\1069140\&7 \RS\1006298B\131854\20746"}), inInviteePhone = Nothing}
testObject_Invitation_team_5 :: Invitation
testObject_Invitation_team_5 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-10T19:03:20.212Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = ".", emailDomain = ""}, inInviteeName = Just (Name {fromName = "gg:,0\7944\ENQ\EOT\n]m\fU\DC4_u\td\ENQ"}), inInviteePhone = Just (Phone {fromPhone = "+007642906492663"})}
testObject_Invitation_team_6 :: Invitation
testObject_Invitation_team_6 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-08T11:25:58.680Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))), inInviteeEmail = Email {emailLocal = "", emailDomain = "p\7920\67286"}, inInviteeName = Just (Name {fromName = "%G\1042221\186280\EOT\DLE\ETB\29493\992976Z\1049681\&7o;\NAKj\"~'vT\GS\RS2\1010326Ec7\1065376?15\EM}\SUB'bd\175444\1061973\a\1047465}@\GS2\1103630cl\1093038\51269iy\DEL{\DC1\DELL%c4Y7\19281K\128076e#\DLE\SOHZ\163526B\997224"}), inInviteePhone = Just (Phone {fromPhone = "+95384728540702"})}
testObject_Invitation_team_7 :: Invitation
testObject_Invitation_team_7 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000000"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-07T01:30:51.247Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), inInviteeEmail = Email {emailLocal = "\GS\DC2Z", emailDomain = "\190397"}, inInviteeName = Nothing, inInviteePhone = Just (Phone {fromPhone = "+12420964279809"})}
testObject_Invitation_team_8 :: Invitation
testObject_Invitation_team_8 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T04:09:58.258Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000002"))), inInviteeEmail = Email {emailLocal = "", emailDomain = "}"}, inInviteeName = Just (Name {fromName = "z\SOHA\7914\&26\13128i`\DC1\1016350Ad\1099791XcWz\b,\DLE\173095iv\SOH\GSOyPD`^\ACK\1060437\1042523\DC1lh\STX\CAN#2A4.^T\EOT\5363\986405S\"#q\SOfP|E\11319b\1084120\r~u\ACK\1008207\1107471!\DC1\158484\&4[\SI\1027311p;\139414\NUL\74634\169725|\1110738\83427\140044\DC3\DEL\DC49\29552\26791Ni\1032383\77907"}), inInviteePhone = Just (Phone {fromPhone = "+767737799956996"})}
testObject_Invitation_team_9 :: Invitation
testObject_Invitation_team_9 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-11T00:42:36.522Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))), inInviteeEmail = Email {emailLocal = "I", emailDomain = "\1035973g"}, inInviteeName = Just (Name {fromName = "P\1057610\SI\1096152\GS4M#(\CAN\FS\SUB9\1018990\FSwR]\SIM\96968p*\66743Ca\aF"}), inInviteePhone = Nothing}
testObject_Invitation_team_10 :: Invitation
testObject_Invitation_team_10 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-07T01:29:26.280Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))), inInviteeEmail = Email {emailLocal = "V:", emailDomain = "T\167147\181086"}, inInviteeName = Nothing, inInviteePhone = Just (Phone {fromPhone = "+71627787797993"})}
testObject_Invitation_team_11 :: Invitation
testObject_Invitation_team_11 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000002"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-06T21:21:10.266Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000000"))), inInviteeEmail = Email {emailLocal = "O+V", emailDomain = ""}, inInviteeName = Nothing, inInviteePhone = Nothing}
testObject_Invitation_team_12 :: Invitation
testObject_Invitation_team_12 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-12T09:25:10.958Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), inInviteeEmail = Email {emailLocal = "p\SI", emailDomain = "\r<\121315"}, inInviteeName = Nothing, inInviteePhone = Nothing}
testObject_Invitation_team_13 :: Invitation
testObject_Invitation_team_13 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T16:27:07.763Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "\1028062\ETB", emailDomain = "c"}, inInviteeName = Just (Name {fromName = "\20500~Z\bG\SIx=\txzg\r\ESCgK\1091846\174578\&3\EOTL\1008657H\1033157\SO\FSd*.\75025\991106\1068366{\NAK\RSA\STXU\45926\NUL\ACK\GSO,\1089110\r\1002884\61765G"}), inInviteePhone = Nothing}
testObject_Invitation_team_14 :: Invitation
testObject_Invitation_team_14 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000000"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-12T03:14:24.016Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), inInviteeEmail = Email {emailLocal = "5", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\13062\178451\126091:\147178rT\120896\1080377\&5W\36473y,:\9864\43782\176902\"6BLVSDpN\1100710tS\STX,\1035105o 4\ACK\DC2\15119\3807j\DC4/f\25642\33685qI\169059Kb$aYQ*\1109160vq\1045154T\t9ND\992765\r\124932-\1015043\f[T\50176i"}), inInviteePhone = Just (Phone {fromPhone = "+03306899012"})}
testObject_Invitation_team_15 :: Invitation
testObject_Invitation_team_15 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-11T14:55:28.755Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "\t", emailDomain = "\b\DLEo"}, inInviteeName = Just (Name {fromName = "\188601\1020224_2Q\186269\&3JpV\DEL/\990615M<\DC3Z\"\121091\1044568\144715o`\1039755\&3\1108929L\1030610t%)\CAN\DC1\f\SO\994596#\GS\DC3xKUud\SOH\993626\CAN\RS\1094596_J\1041981C,#1\988258\161359B\1059206\NULq\SUB0\SOHdr\FSyg,\v-1T\1094051p\RS\1012169vVYxD@\985051\ENQ\97381\ACK"}), inInviteePhone = Just (Phone {fromPhone = "+91875841"})}
testObject_Invitation_team_16 :: Invitation
testObject_Invitation_team_16 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-10T12:23:38.330Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))), inInviteeEmail = Email {emailLocal = "np\997920", emailDomain = ""}, inInviteeName = Just (Name {fromName = "w\\dY\ACKsR\a\163242G\985835^_u\132890\ESC6\137928Q\ENQ\165635\148359\DEL%\1020279K*\DC1:O83\165692<a"}), inInviteePhone = Just (Phone {fromPhone = "+311116398012273"})}
testObject_Invitation_team_17 :: Invitation
testObject_Invitation_team_17 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-07T19:59:04.992Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))), inInviteeEmail = Email {emailLocal = "C", emailDomain = ""}, inInviteeName = Nothing, inInviteePhone = Just (Phone {fromPhone = "+765223468048"})}
testObject_Invitation_team_18 :: Invitation
testObject_Invitation_team_18 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-08T19:52:43.702Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\ETB9S5LOLh:\35661b\181666\120987\RSK\1036280u\98778\&6\1064464E)\GSm\1081192\1036257\ACK\120342*\166071vv\194652\1062990'KQ\ENQD\162158\ETBK\173366UN/\DC4.[\26910\EM\1085758\ESC\150086\1042830~\18673<"}), inInviteePhone = Nothing}
testObject_Invitation_team_19 :: Invitation
testObject_Invitation_team_19 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-08T14:40:19.989Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Nothing, inInviteePhone = Nothing}
testObject_Invitation_team_20 :: Invitation
testObject_Invitation_team_20 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-07T14:41:48.851Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002"))), inInviteeEmail = Email {emailLocal = "?", emailDomain = ""}, inInviteeName = Nothing, inInviteePhone = Just (Phone {fromPhone = "+8453733683162"})}
