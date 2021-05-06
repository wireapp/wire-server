{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.InvitationRequest_team where

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
testObject_InvitationRequest_team_1 :: InvitationRequest
testObject_InvitationRequest_team_1 = InvitationRequest {irLocale = Nothing, irRole = Just RoleExternalPartner, irInviteeName = Just (Name {fromName = "eT\ESC\US\FS\SO\151097B\36642\166118{\CANh\158821\a\59295(\b\1005528\GS\26745yd\110971\n\1107475K[w\NAK\41597\129544#dAMtt\EOT0\1088456\&1\63923\133981I1ZZj :\GS\159507=~\ESC?+g2Lgd\1108880{:\v\GSA\985308\133517#SO5a\DC4?F\30486\EM\984428N5\NUL\1037527wz\179982 3\NAKzvn\31506fK\1056905\100439\NUL\1063943\DC4=\SI3Wdo\GS\46411"}), irInviteeEmail = Email {emailLocal = "\1068138\&2\"", emailDomain = "~7\DC3"}, irInviteePhone = Just (Phone {fromPhone = "+75835926156"})}
testObject_InvitationRequest_team_2 :: InvitationRequest
testObject_InvitationRequest_team_2 = InvitationRequest {irLocale = Nothing, irRole = Just RoleAdmin, irInviteeName = Just (Name {fromName = "B+\169049\GS(\159471\&5`{YO!\n\RS\15942\178041\161328?;\1015708\ETX\DC1\DC39\DC1\987630\DC3\SOH\NAK\127367K\157846\ETXcF\t\49813/SV]\1098530#Z\DC4\54859jKm\ENQ\DC1`\167443\59943\1070375\1057496gF\10092|\tl\157368&\32159"}), irInviteeEmail = Email {emailLocal = "7=\1012664", emailDomain = "\n1"}, irInviteePhone = Just (Phone {fromPhone = "+99292290"})}
testObject_InvitationRequest_team_3 :: InvitationRequest
testObject_InvitationRequest_team_3 = InvitationRequest {irLocale = Nothing, irRole = Just RoleMember, irInviteeName = Just (Name {fromName = "G#6f\SI\SUB2\1075140\&4\1014823 &%D\1030584F\SOH\DC13j\35762tA#\143545\ACK<\159645\SUBU\SOH\22752\41544ee\FS\1087112\69948qs\GSB\159633?\n \ETX\r\t}\1071338N\1079356\SOH1zK\RS\bwQ\b1|z.\135049T\157690\bI\59509/cy\67413\DC2\RSy\1101357|\70002\1108566\1000399E{\994136n\1061036\&3\32570W\"f/=\1014115aX\EOT\100725Z+\11944Nj\128286'"}), irInviteeEmail = Email {emailLocal = "+S:\EOTX", emailDomain = "{.E"}, irInviteePhone = Nothing}
testObject_InvitationRequest_team_4 :: InvitationRequest
testObject_InvitationRequest_team_4 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.CO, lCountry = Nothing}), irRole = Just RoleExternalPartner, irInviteeName = Nothing, irInviteeEmail = Email {emailLocal = "[\1104928U\136426.", emailDomain = "g\150284"}, irInviteePhone = Just (Phone {fromPhone = "+83640643647593"})}
testObject_InvitationRequest_team_5 :: InvitationRequest
testObject_InvitationRequest_team_5 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.TK, lCountry = Nothing}), irRole = Nothing, irInviteeName = Nothing, irInviteeEmail = Email {emailLocal = ".", emailDomain = ""}, irInviteePhone = Just (Phone {fromPhone = "+2315064499"})}
testObject_InvitationRequest_team_6 :: InvitationRequest
testObject_InvitationRequest_team_6 = InvitationRequest {irLocale = Nothing, irRole = Just RoleAdmin, irInviteeName = Just (Name {fromName = "e>\44917\78743\1110126K\73976P:\EM~\186754\&4Q\NAK\FSi"}), irInviteeEmail = Email {emailLocal = "\121270", emailDomain = "`"}, irInviteePhone = Nothing}
testObject_InvitationRequest_team_7 :: InvitationRequest
testObject_InvitationRequest_team_7 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.KY, lCountry = Just (Country {fromCountry = GE})}), irRole = Just RoleMember, irInviteeName = Just (Name {fromName = "\1084340\&8\1059516\DC2p\"\1067940\ESC\987339\8325U\DC1\150502W\EM(\ETX\vlMhl\182537|\1079354HaGA\60912\&4\f\DEL\1026506\54210\NULrI[R;\66583]\ESC\EOTcS}_gO\62611hevz\\#!a91\ACK\NAK\ESC\n\FS\STX\182031\&3}t\1113455\ETXqC\1053444\ACK\STX>|`\168019x\179481]t0\95458\b+,\1065565\45899\153396J7O\f\19784)wa3"}), irInviteeEmail = Email {emailLocal = "D", emailDomain = "c\tf\US\STX\150634"}, irInviteePhone = Nothing}
testObject_InvitationRequest_team_8 :: InvitationRequest
testObject_InvitationRequest_team_8 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.AN, lCountry = Just (Country {fromCountry = AM})}), irRole = Just RoleMember, irInviteeName = Nothing, irInviteeEmail = Email {emailLocal = "", emailDomain = "y`f\ETB\160090"}, irInviteePhone = Just (Phone {fromPhone = "+853030720251995"})}
testObject_InvitationRequest_team_9 :: InvitationRequest
testObject_InvitationRequest_team_9 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.ML, lCountry = Nothing}), irRole = Nothing, irInviteeName = Just (Name {fromName = "S\2011"}), irInviteeEmail = Email {emailLocal = "\1015588", emailDomain = "\SI"}, irInviteePhone = Just (Phone {fromPhone = "+298685385757"})}
testObject_InvitationRequest_team_10 :: InvitationRequest
testObject_InvitationRequest_team_10 = InvitationRequest {irLocale = Nothing, irRole = Nothing, irInviteeName = Just (Name {fromName = "7Vu1SLP\US>]\RS\127868\35414\5081x{%\51891\32549\30523\51297\\\1024245\&9\992883V\1000587Hf\NAKo?ld\44056\1050499`#\168141q\20402\SYN5\98358k\US\SObC\1054425RK[Ov\51260\DC2t"}), irInviteeEmail = Email {emailLocal = "%\176537#", emailDomain = "/"}, irInviteePhone = Just (Phone {fromPhone = "+324499571519418"})}
testObject_InvitationRequest_team_11 :: InvitationRequest
testObject_InvitationRequest_team_11 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.GU, lCountry = Just (Country {fromCountry = PE})}), irRole = Just RoleAdmin, irInviteeName = Just (Name {fromName = "6y)\"\168934\DLE\3840`$\94699\SOH\n\151659&\38136/d;\ETB_9\94627\&7h\1103050.\100141\44660\SOH\95500\38740\"\69714\1055332\&3=\43288l;\995352\1070881wD<\1962O.N&|e\15603\EMZ\1069907es5\EMr\1041825\\\25447t"}), irInviteeEmail = Email {emailLocal = "1Bx\RS;", emailDomain = ""}, irInviteePhone = Nothing}
testObject_InvitationRequest_team_12 :: InvitationRequest
testObject_InvitationRequest_team_12 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.RN, lCountry = Just (Country {fromCountry = MW})}), irRole = Nothing, irInviteeName = Nothing, irInviteeEmail = Email {emailLocal = "q\NUL", emailDomain = "\SI+"}, irInviteePhone = Nothing}
testObject_InvitationRequest_team_13 :: InvitationRequest
testObject_InvitationRequest_team_13 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.KR, lCountry = Just (Country {fromCountry = GA})}), irRole = Just RoleExternalPartner, irInviteeName = Nothing, irInviteeEmail = Email {emailLocal = "fmxW", emailDomain = "\917579\45384B!\NUL+"}, irInviteePhone = Just (Phone {fromPhone = "+884219966887364"})}
testObject_InvitationRequest_team_14 :: InvitationRequest
testObject_InvitationRequest_team_14 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.EO, lCountry = Just (Country {fromCountry = IN})}), irRole = Just RoleMember, irInviteeName = Just (Name {fromName = "_I\ENQ\65945\GS\1231PD\1055956 \GS\11297\151113oE\EM-C\"e\DC4\1006935u\SOHO\1088331r\186335d\SYN&\1016188\166905\&2N\GS`\73101!M`\SYNk\SUB he\1070777+P~ni\DC16\146111\1009677\357\135144\t\31437\37978\&7^%\SUB\175681f\1098582\&0\\\DC2opk\1023653"}), irInviteeEmail = Email {emailLocal = "\989513wU\SI\150326", emailDomain = "00,\144110\&3"}, irInviteePhone = Just (Phone {fromPhone = "+7304932691993"})}
testObject_InvitationRequest_team_15 :: InvitationRequest
testObject_InvitationRequest_team_15 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.CR, lCountry = Just (Country {fromCountry = BD})}), irRole = Just RoleExternalPartner, irInviteeName = Nothing, irInviteeEmail = Email {emailLocal = "f)+KB", emailDomain = "Fw"}, irInviteePhone = Just (Phone {fromPhone = "+8655787281"})}
testObject_InvitationRequest_team_16 :: InvitationRequest
testObject_InvitationRequest_team_16 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.KK, lCountry = Just (Country {fromCountry = DE})}), irRole = Just RoleOwner, irInviteeName = Just (Name {fromName = "\177948\CAN\DC1\1059725K{\1054952jz{\159568\a,\1083951\ETBu\NUL,\34066\65928;l\RSX\USP,\188143*\1068260\145311I\1017729\162356Ab\187735\&0\ETB~\ESC"}), irInviteeEmail = Email {emailLocal = "r:\"\DC1", emailDomain = ""}, irInviteePhone = Just (Phone {fromPhone = "+518336159896"})}
testObject_InvitationRequest_team_17 :: InvitationRequest
testObject_InvitationRequest_team_17 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.MH, lCountry = Just (Country {fromCountry = SY})}), irRole = Just RoleOwner, irInviteeName = Just (Name {fromName = "\1109048.\10413|v\USWl69O[\159276\&3\ACKd\f \55219\98580_\US\"?\DC23\1102636x$\ESCnDMn-kx\"\1054611\1013509\a\1110387'\1086730r\169874\DC1XU"}), irInviteeEmail = Email {emailLocal = "\1105102", emailDomain = "vaA\17435z\DC2"}, irInviteePhone = Just (Phone {fromPhone = "+736027213"})}
testObject_InvitationRequest_team_18 :: InvitationRequest
testObject_InvitationRequest_team_18 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.TG, lCountry = Just (Country {fromCountry = AR})}), irRole = Just RoleExternalPartner, irInviteeName = Nothing, irInviteeEmail = Email {emailLocal = "\CAN", emailDomain = "Z"}, irInviteePhone = Just (Phone {fromPhone = "+836594795623"})}
testObject_InvitationRequest_team_19 :: InvitationRequest
testObject_InvitationRequest_team_19 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.TT, lCountry = Just (Country {fromCountry = VG})}), irRole = Nothing, irInviteeName = Just (Name {fromName = "\DEL>!\138470Pr\1005360\179958@\NUL<\DC4\1027354Z\DC4\v4\SOr\1096387\SUBT\1069909naN h\11975|eB^\1036337\vf:f\146233\151802M\1015195\US\179961mS$/\999576\&1=t\60787j\v\NAK\1102578\142226X\FS:mN\993359\131885"}), irInviteeEmail = Email {emailLocal = "\1069976,\72861x", emailDomain = "o\RS^\54048"}, irInviteePhone = Nothing}
testObject_InvitationRequest_team_20 :: InvitationRequest
testObject_InvitationRequest_team_20 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.BH, lCountry = Just (Country {fromCountry = CK})}), irRole = Just RoleOwner, irInviteeName = Nothing, irInviteeEmail = Email {emailLocal = "\DC4\n\"\1006654%", emailDomain = ""}, irInviteePhone = Just (Phone {fromPhone = "+68879173456731"})}
