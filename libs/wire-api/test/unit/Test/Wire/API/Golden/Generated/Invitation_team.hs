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
testObject_Invitation_team_1 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-07T08:46:50.714Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002"))), inInviteeEmail = Email {emailLocal = "m<", emailDomain = "\CAN-$"}, inInviteeName = Just (Name {fromName = "\SOaTE?\EM\21146oY\50481;z\1045505Vc\n\DELs\DC2\53603r+!N6R|\SOH\1090204\DELI"}), inInviteePhone = Just (Phone {fromPhone = "+572415487761932"})}
testObject_Invitation_team_2 :: Invitation
testObject_Invitation_team_2 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T08:47:03.633Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))), inInviteeEmail = Email {emailLocal = "\DC3\985547", emailDomain = "\1013944\EOT"}, inInviteeName = Just (Name {fromName = "A\128478\58243w_\DEL\1014974*|\STX\NAK\2708$x\184696GG\16004\1038815oo\7910\1025884\179739XB\FS\SO\f9\31964\37845k\175496b\RSU0\b-\SUB3LZrr\bx\1101629l\1103354 W1.PC\94856G\180144\126221\1109273\1077943)\127923\1107319\1008326\ETXQI\30957@\1049272\69646i\998926\ETB:+y\1014898(\SYNJT\\\13687e&\1071646\ENQ"}), inInviteePhone = Nothing}
testObject_Invitation_team_3 :: Invitation
testObject_Invitation_team_3 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000001"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-06T14:08:33.464Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "kD", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\f_#PN\FS\DC4Y\997776\147766\FS5\ftE\CAN\171105\DC1A_Xd:)\DC3\ETB0\1015295\33365J=R:Y\165279 \148706\146298\SO\CAN\SOH\95749\nJAIh\n4P\18646'\1063690\SOH\67674<L\151886\DC3\180746\ETBW9\988582\1027403*B\1107156n\";Nj'\1013014@[E"}), inInviteePhone = Nothing}
testObject_Invitation_team_4 :: Invitation
testObject_Invitation_team_4 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000001"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-06T22:02:15.835Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000000"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "m;\DC4+(yrH@\EM\1080470B:d2\DC3\175337\&0M\n\41145q\1017604RXO Y\8162\164345[>\1107250\NULSz\ETB6\184374\1072926Jw\STX;\SYN\1032727/\7257\1044169\r\1081150\51003h\n\1005933>\1070244r$I\136916"}), inInviteePhone = Just (Phone {fromPhone = "+7167545199"})}
testObject_Invitation_team_5 :: Invitation
testObject_Invitation_team_5 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000002"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-07T11:37:01.765Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "w\ENQ", emailDomain = "X"}, inInviteeName = Just (Name {fromName = "~\FSL\1052882h\STXTp\1022726\aOB7\7815\1075812m\DELE'RD\996657hx\185984I\ETX\140028\1078309\v\1025866\168265n\DC4)_J\59484"}), inInviteePhone = Just (Phone {fromPhone = "+10303278146"})}
testObject_Invitation_team_6 :: Invitation
testObject_Invitation_team_6 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-12T01:53:04.140Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "]\\9", emailDomain = "\v)"}, inInviteeName = Just (Name {fromName = " \1004410>8W\a\"\123594M&\EM\25415 *\23779\ETB\EMH\11396v_\1037146\DC2q-24\DLE\SYN@\1084245\NAKi|qv\NUL\CAN\t\12296\986229\1009060MZ<\42115\1012618\1062669eE\FS\DC2\NULb\1020407(Z\998459\142998\27196\22779\1103885\&4IC2\v\STX?\n\rT\1111143-'{U\fj+\61171\f\f\146287\27112]d%53S\RS\STX\92580e\161584\154218h\SUB\SOHY"}), inInviteePhone = Just (Phone {fromPhone = "+9045384732"})}
testObject_Invitation_team_7 :: Invitation
testObject_Invitation_team_7 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-12T11:32:17.237Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "\96111\GS", emailDomain = "\990693h"}, inInviteeName = Nothing, inInviteePhone = Just (Phone {fromPhone = "+586271303"})}
testObject_Invitation_team_8 :: Invitation
testObject_Invitation_team_8 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-12T23:37:18.437Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "E?", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\169660K?'hqNSK_zioH6\1095255\1036305\DEL\128257\n|\SO\1056674\DC4A\1087452\126596fc\USK\"~lN1B\DC3\ENQ\94890\SYNI\131104*x|\1068402_\NULuI\EOT5Ro\r\NAK\SO\1059800"}), inInviteePhone = Nothing}
testObject_Invitation_team_9 :: Invitation
testObject_Invitation_team_9 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000002"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-10T05:44:57.839Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "\36346", emailDomain = "'\ACK"}, inInviteeName = Just (Name {fromName = "%>R\fq0\1042122w/\1094104eh\DEL\1012041K\v\r\1002801\DC1F\174815c\127197N0\fX\DC4Q\DC2\SYN\ENQ]cA 3\1061801\SI\ENQQ\GSxa\SI\EOT+\53549\&11s\ETBOhq\a-gnQ\100038g\1034529;\59686#\162281\134116\1005903w\ENQ\6187j\162728\&8O\134650\54425c\19033l\DC3\DC1\82973\SYNV\1111422\NUL\SOF\ETX\48536t\v\EOT2A\7074\1012254KYK~\138078\&5\f9gl:s\ENQo\t\6257W="}), inInviteePhone = Just (Phone {fromPhone = "+4362634730246"})}
testObject_Invitation_team_10 :: Invitation
testObject_Invitation_team_10 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T04:39:01.852Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))), inInviteeEmail = Email {emailLocal = "B", emailDomain = "\132051\ENQ\182677"}, inInviteeName = Just (Name {fromName = "m\995764\USGy'I\ETX\8932\&7\SUBd|f_^K\138133Ff\39469\1054274x&z\US\SOH#\802s\US\1113196\r\1082771\GS!U\CAN\18394\149966\&6=!\b]\NAK\CAN\vS~8P:P\1041595\1016018-,\177467V\SYN\1047383r%Nd\SUB\137329\a=C\tjF0)Q\69458\&6\SIQ\1060687\ACK\189266^\163600)\RS)\\1 K.m=$\DLE\49893.TR\162698IZ\174388H+18\t\FS\DELL/T"}), inInviteePhone = Just (Phone {fromPhone = "+1194808436353"})}
testObject_Invitation_team_11 :: Invitation
testObject_Invitation_team_11 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-11T16:11:01.616Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), inInviteeEmail = Email {emailLocal = "9\1065289", emailDomain = "SV"}, inInviteeName = Just (Name {fromName = "\1099124\ENQ\DC22\DC1B\ETXX\164634\"F\1051637.\nN\1028373\139566\&7V\NUL\fmo\rQ\ETX1)BBfoX\100421yoj\1010780\b\187761K\120699\GS\"`#+\GS]\DELJ\1075593g\1086446\ACK~\24475\101004D\997600&}\33259\SOH\21668<,\n\191413I\1111803'NEA\1010172\9687D\1111036\20671\1081225(IK<H\1041677jJI\172468sl\1069250\1051624=J\ENQo\b\30245\1004331^-TY\1028822\111240\989255\&3]#\CAN|\1002040cg\1051622"}), inInviteePhone = Just (Phone {fromPhone = "+98222831"})}
testObject_Invitation_team_12 :: Invitation
testObject_Invitation_team_12 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-12T07:08:39.543Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))), inInviteeEmail = Email {emailLocal = "\152232\&1", emailDomain = "\DLExI"}, inInviteeName = Nothing, inInviteePhone = Just (Phone {fromPhone = "+9527919364896"})}
testObject_Invitation_team_13 :: Invitation
testObject_Invitation_team_13 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-11T22:56:02.551Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "C\DC1KV]yLzbPj6\EMorv\141911R18\EMz&<\NAKr\996615\1098889o]e4\1106928w\EMAF\176147c\141656\DEL\EMde}\138807\38754(+NA1]\RS\DC2\74425\a]R\f5\RSj\NAK\t\CAN2/\171150=G!Y\1059660=\1085928XF>H\148987Dz\1068495\&7~[Am\132954\&7JY\1049811\1050514\1063933Gw"}), inInviteePhone = Just (Phone {fromPhone = "+63150361"})}
testObject_Invitation_team_14 :: Invitation
testObject_Invitation_team_14 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-08T11:28:31.486Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), inInviteeEmail = Email {emailLocal = "x\162313", emailDomain = "h"}, inInviteeName = Nothing, inInviteePhone = Just (Phone {fromPhone = "+011272109885127"})}
testObject_Invitation_team_15 :: Invitation
testObject_Invitation_team_15 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-07T22:55:03.371Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000002"))), inInviteeEmail = Email {emailLocal = "\1048513", emailDomain = "0j\1080036"}, inInviteeName = Just (Name {fromName = "\DLEs\DC4\US\DLE\182557A\EMY\1024927nPw\DC3m{\1013183\&9~U^0n%\NULV^4d\ESC\1096280~j\USZx\b\151821\ETXPuz|\29308\1054060\1050500\37715A\19741j\ACK\NULGh\a|\131832x4\7855\1058117z#Q\1046279Z\52295\159407\1105892o\SI~\"\1838am*6\1023270\NAKCi\ENQt\9347\1025092m\DC3f.\\@\ACK\\Y\1076712j\25877-.^=\985188\bH}L{\FSc"}), inInviteePhone = Just (Phone {fromPhone = "+5929607757455"})}
testObject_Invitation_team_16 :: Invitation
testObject_Invitation_team_16 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-06T07:38:28.284Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001"))), inInviteeEmail = Email {emailLocal = "\1033646", emailDomain = "\rF"}, inInviteeName = Just (Name {fromName = "%\RSg\t\DC3.o\SI_=;\1025344k6\ESC\DLE\1045103Cq\1007368\174881Xb\1058168n\24401Q\1006521\125119, 1"}), inInviteePhone = Just (Phone {fromPhone = "+44412005423261"})}
testObject_Invitation_team_17 :: Invitation
testObject_Invitation_team_17 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T14:28:45.391Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))), inInviteeEmail = Email {emailLocal = "\997908", emailDomain = ""}, inInviteeName = Just (Name {fromName = ">\ETXPN@\78213\1727Vw/%\180132\CAN\1081701\74048\DELJ\1064124\FS\3946s.<v{\NAK|"}), inInviteePhone = Nothing}
testObject_Invitation_team_18 :: Invitation
testObject_Invitation_team_18 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-11T00:22:31.280Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = "+\GS^"}, inInviteeName = Just (Name {fromName = "fX7c\64570Bd\1004509*~?2f&\139733\1042448p%(Ri\64928\1104183\ENQR 74,h\CAN\174378\&2a>k6Jy.mo,\138284\39999h8h`;\b<\FSe>^\10934\SUB\"\t\DC1\158300\1079373\1068025\EM\DC4>n3\NULs\1041053\177263hn\51883\178187\1007317Ot>\1082342u\r"}), inInviteePhone = Just (Phone {fromPhone = "+96003032285970"})}
testObject_Invitation_team_19 :: Invitation
testObject_Invitation_team_19 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000000"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-08T13:20:16.263Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))), inInviteeEmail = Email {emailLocal = "A", emailDomain = "Ads"}, inInviteeName = Just (Name {fromName = ",@=\DELj\NUL\1083728B\173344L\NAK0/\138321\"\ESC\1010509iW\7555o\b\141626v\SO\179933f#\1051111[g|/\996964a<\60224~:FI\184713\995043_@\b\r>\f\1031921BX@)\DC4zc\DC12\185949\SYN*j`\40803\120902\SYN&<<,\189166\1097060"}), inInviteePhone = Just (Phone {fromPhone = "+40718666"})}
testObject_Invitation_team_20 :: Invitation
testObject_Invitation_team_20 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-07T23:47:04.976Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = "\r"}, inInviteeName = Just (Name {fromName = "8g\142730\1019539!C\61951\RS\170406\29396\&9K\26424\ESC\r"}), inInviteePhone = Just (Phone {fromPhone = "+4459496937465"})}
