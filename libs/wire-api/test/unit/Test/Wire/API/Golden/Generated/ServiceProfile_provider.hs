{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ServiceProfile_provider where
import Data.Id ( Id(Id) )
import Imports
    ( Bool(False, True), Maybe(Just, Nothing), fromJust )
import qualified Data.UUID as UUID ( fromString )
import GHC.Exts ( IsList(fromList) )
import Wire.API.Provider
    ( ServiceTag(VideoTag, BusinessTag, TravelTag, HealthTag,
                 DesignTag, FitnessTag, GraphicsTag, NewsTag, EntertainmentTag,
                 AudioTag, RatingTag, GamesTag, PollTag, ProductivityTag,
                 FinanceTag, MoviesTag, ShoppingTag, QuizTag, SocialTag, WeatherTag,
                 BooksTag) )
import Wire.API.Provider.Service ( ServiceProfile(..) )
import Wire.API.User.Profile
    ( Name(Name, fromName),
      Asset(ImageAsset),
      AssetSize(AssetPreview, AssetComplete) )

testObject_ServiceProfile_provider_1 :: ServiceProfile
testObject_ServiceProfile_provider_1 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002"))), serviceProfileName = Name {fromName = "\v|cz1\99190I!O-\CAN\4741\ESC\SUB\n\1063563\NUL\1045000\1111205?%W\132803\&6\38447\&5\34926\1039318a.`+HH\24240\"\1075901\&5\1031191W\1020904\1096054!wuZ\54039\1069041\DC2L\1063515\NAK?0h4 \987951T%\187587.\145779\1029855'm\1045029^vI*\ESC\16556\&1:\1056123t\SYNr"}, serviceProfileSummary = "\7754", serviceProfileDescr = "", serviceProfileAssets = [], serviceProfileTags = fromList [], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_2 :: ServiceProfile
testObject_ServiceProfile_provider_2 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000002"))), serviceProfileName = Name {fromName = "t\ACKs\1086773\SUBF\ESC\35906\\\SOeI\48228\ETB/I\1104438\r\1039847p\DC4`\DC1>Z\1057905\989933^L\68374Ul\GSNY\1032316\DC1\45435E8\181228\"h\1007796>\1038524\1009623#*q<d\1011385\1064167\STX`9X\DEL\CANFN]^_l"}, serviceProfileSummary = "\ETXe", serviceProfileDescr = "z\"", serviceProfileAssets = [], serviceProfileTags = fromList [QuizTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_3 :: ServiceProfile
testObject_ServiceProfile_provider_3 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), serviceProfileName = Name {fromName = "\118995rAo\ACK\78309\EOT{\48259\EM'A\ACK\70846\ETX\SUB\1061628VRE%%H{\a\1065287\22991\1037754NOeh,\NULf\145475Z\ENQ\ACKU\ESCSZ\1082050Tojz\1051387\155608\166419E{\1053448An\5669(%TJ\1011980-'\1020450\STX\b_ysd\1033283\157201\998951y\SYN\186457\44626zLo\1035596[_)E\1109188\185701l\SOHk3\1014452F,\DLE'\DELpn\1105330\v%\121440'IW\1037657S=\f\29521-{\1094000\158472\1068795j\r"}, serviceProfileSummary = "\182450t\SOH", serviceProfileDescr = "Hs", serviceProfileAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview))], serviceProfileTags = fromList [DesignTag,ProductivityTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_4 :: ServiceProfile
testObject_ServiceProfile_provider_4 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001"))), serviceProfileName = Name {fromName = "s\1046715$\78441\163996\1076991\100324i3\25451s~\DC2.0p\1065615M\36064\DC3`}\162226<\"5U\ESCfsx\1003958\144106\SUBa\SOI\a\ACKVU\DC3V\STX\140723r\DLE\997001\FSt\RSg3R\EOT\456\1074454\1029355\1080411\23338a}*e\1060254"}, serviceProfileSummary = "X\989700", serviceProfileDescr = "", serviceProfileAssets = [], serviceProfileTags = fromList [RatingTag], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_5 :: ServiceProfile
testObject_ServiceProfile_provider_5 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))), serviceProfileName = Name {fromName = "\STX|\1030963\60248\&1r{\DC4vx\1103180f+\181803[&MN\63628\&3\NUL_q;}\t\"@~(sOe\1076086\SOkL*~\184755\118874\1013092\1110368\ETB*\b\44636-W\"&L\999581A8v\1037420\DLE\984176\&0\173865\14179!\1053627\r)\30198y\119526\US\EOTt>*\1008620\188695\DLE\"\DC3\1043082\&2\rs;J\\\8991c \FSF\vD7\"n^"}, serviceProfileSummary = "\FS", serviceProfileDescr = "", serviceProfileAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Nothing))], serviceProfileTags = fromList [BusinessTag,MoviesTag,TravelTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_6 :: ServiceProfile
testObject_ServiceProfile_provider_6 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), serviceProfileName = Name {fromName = "\1010030f\49610\177396\168482\1045520v\26870hH@'\DLE1}H\1068723a\1046106H\ENQ\34145GAn\66443\&0\EOT5D\SOH\SI\996504*SK#hy\b*B\NAKcj\1024994$Mv1. =G\r_\ENQE\DC1y;\1026885?\b\DEL\96061\GS\ACK& \180058*m\f.~\14141L\1068206\72161V\1042530h\166014\&8\1052315\EOT\GS\SO\42051}w> \1033171\RSC[\1020791/nqotj\n\1023169W?~\1033355R\v}\32453)AMH\8212\2846^"}, serviceProfileSummary = "\US", serviceProfileDescr = "\97415/O", serviceProfileAssets = [(ImageAsset "" (Just AssetPreview))], serviceProfileTags = fromList [BooksTag,HealthTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_7 :: ServiceProfile
testObject_ServiceProfile_provider_7 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000002"))), serviceProfileName = Name {fromName = "\v0Su\31820&\1047165^dC!$\1030835\40812l\DC3\151541Y\74331<>\184184~&F\119126b{x`A\154520K\1075897\&0\1095977"}, serviceProfileSummary = "\1051296", serviceProfileDescr = "E\10658", serviceProfileAssets = [(ImageAsset "\"" (Just AssetComplete))], serviceProfileTags = fromList [AudioTag,FinanceTag,QuizTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_8 :: ServiceProfile
testObject_ServiceProfile_provider_8 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))), serviceProfileName = Name {fromName = "\191091\DEL"}, serviceProfileSummary = "", serviceProfileDescr = "", serviceProfileAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview))], serviceProfileTags = fromList [DesignTag,ProductivityTag,SocialTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_9 :: ServiceProfile
testObject_ServiceProfile_provider_9 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))), serviceProfileName = Name {fromName = "B\6098|Q\ACK}RN~c\174243\SIuA\988762u\146825\43645,Y*\1004435q\NAKp=\49878|\"`M\"\DEL\RSG\157846\DC3t\t]U\DLE\1014038$uuC"}, serviceProfileSummary = "\b\FS|", serviceProfileDescr = "", serviceProfileAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview))], serviceProfileTags = fromList [PollTag,SocialTag,VideoTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_10 :: ServiceProfile
testObject_ServiceProfile_provider_10 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000000"))), serviceProfileName = Name {fromName = "\1082547\EOT5:.l"}, serviceProfileSummary = "\174974\121083\1040712", serviceProfileDescr = "P\1063450\NAK", serviceProfileAssets = [], serviceProfileTags = fromList [FitnessTag,GraphicsTag,NewsTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_11 :: ServiceProfile
testObject_ServiceProfile_provider_11 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000"))), serviceProfileName = Name {fromName = "\992059\ENQg~q\168079\1088983bF|\28540\168067GTx\SOH\456\ETX`g\\\SYN55\158059\1092809h\1032038Zo 1\f\127314m\10936tdU\1019213\aM\DC1u\165062\20099\&0K"}, serviceProfileSummary = "\1016180c", serviceProfileDescr = "R8", serviceProfileAssets = [], serviceProfileTags = fromList [EntertainmentTag,ShoppingTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_12 :: ServiceProfile
testObject_ServiceProfile_provider_12 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000002"))), serviceProfileName = Name {fromName = "\1001062c`Kc\170919\&0O\SIB\1087117\t\1039739k\f+05\29400v{ '\EOTl\bi\DEL\ESC-\1105427\18066\1098615Z\172155\18435\SOk\NULti{%}&#\151838\1083590\SYN\ETBt\180685=jvwAF\18025$|rA\ENQ\190940\&4*\1103678p"}, serviceProfileSummary = ".\r\SUB", serviceProfileDescr = "u?4", serviceProfileAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Nothing))], serviceProfileTags = fromList [AudioTag,FinanceTag,RatingTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_13 :: ServiceProfile
testObject_ServiceProfile_provider_13 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))), serviceProfileName = Name {fromName = "sLPk54\1072404Z\164406\&2EQU\vY[D\51469\ENQ{u2F\SYN\ESCjk\DC2\n1\1098123\1109163f*w||&-\48292\72771\ETB\GS\983049\35048[k\150307W%\US(3n$\DC2S:\EM\169571\15319\EM\ETX(\986044fE'p\DLEyd\1310\DLEQYNX\1066593:\1022248W\1096382E4\1039168<"}, serviceProfileSummary = "qA%", serviceProfileDescr = "", serviceProfileAssets = [], serviceProfileTags = fromList [GamesTag], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_14 :: ServiceProfile
testObject_ServiceProfile_provider_14 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000002"))), serviceProfileName = Name {fromName = "O\b\NAK\998524Qe\148639\1108339A^)=\SOHR6(\NULT\144093\ETX58\22124\1043280\EMLbY\182547\1079554\ETBu\GSz9\SI\1104478\995903\&3\94438A\DEL\2713a\STX{\166987\ACK-\nNx\DC1\DC3}4\nD\SYN\fT/gY\r\1051998\1030308\ETBC0[\5321\132902~6\159460\1016153\10378\DLEHd\153455LxB\167036\48969bm\1060102=\1001169\&3/1q\DEL@Z\168726\163192_"}, serviceProfileSummary = "", serviceProfileDescr = "\1090882*", serviceProfileAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Nothing)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete))], serviceProfileTags = fromList [PollTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_15 :: ServiceProfile
testObject_ServiceProfile_provider_15 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), serviceProfileName = Name {fromName = "A$\NAKy\13352\bP-fi\SYN\54096r\52799<\51822BB\1015229>\173132\186139CF3EhTOa\\5\DLE\DC4\136396\1011890\ESC*Kh\nL\145282E~9"}, serviceProfileSummary = "G\36301z", serviceProfileDescr = "", serviceProfileAssets = [(ImageAsset "3" (Just AssetComplete))], serviceProfileTags = fromList [ProductivityTag,SocialTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_16 :: ServiceProfile
testObject_ServiceProfile_provider_16 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))), serviceProfileName = Name {fromName = "N_&j}Zl2N\NAK\1093457\"U*[Y\1111031\186488e\DC4S\1078019`\985211\DELu\1111033\EM8\1016304\177699"}, serviceProfileSummary = "", serviceProfileDescr = "\SYN\t$", serviceProfileAssets = [], serviceProfileTags = fromList [FinanceTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_17 :: ServiceProfile
testObject_ServiceProfile_provider_17 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000"))), serviceProfileName = Name {fromName = "\EOT\32074^?*p\STXp`(\r\1074094e)\133569d\"\64297{4,gh.\1011313.\\5\UST\38657]i\SO\ACKt|k@M\1012128ic&\173561\FS[{i\f\152961zl\NAK\1069999a\1020881W"}, serviceProfileSummary = "", serviceProfileDescr = "\1024892L\RS", serviceProfileAssets = [(ImageAsset "\ENQ" (Just AssetPreview))], serviceProfileTags = fromList [MoviesTag,ShoppingTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_18 :: ServiceProfile
testObject_ServiceProfile_provider_18 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000002"))), serviceProfileName = Name {fromName = "C,\ETXO\120956\1006915$].\1011868@\ESC\189427\tfW\1102816\f\128815/\152391i\n\DLE9\NAK\1032014\149479j;\\\28120\149468u-\999362af)\\\1075929\"\NUL$\1104453{\1051100\50065\1015757}b\DC1\1860\151329\ETX\EOT\1087047Ze?\1028011JF\SYN}\1002307l\v\SOH\44826r\1009431\ACK\ACKel\r\US\\"}, serviceProfileSummary = "\1086399", serviceProfileDescr = "", serviceProfileAssets = [(ImageAsset "" (Nothing)),(ImageAsset "" (Nothing)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview))], serviceProfileTags = fromList [QuizTag,SocialTag,WeatherTag], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_19 :: ServiceProfile
testObject_ServiceProfile_provider_19 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000001"))), serviceProfileName = Name {fromName = "\NULS"}, serviceProfileSummary = "\132442k", serviceProfileDescr = "\ETX", serviceProfileAssets = [], serviceProfileTags = fromList [BooksTag], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_20 :: ServiceProfile
testObject_ServiceProfile_provider_20 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000000"))), serviceProfileName = Name {fromName = "\ETB\SO(H\160071?\DLE\1024640\a7\30290\133487Dk\24692:b\149734B"}, serviceProfileSummary = "", serviceProfileDescr = "", serviceProfileAssets = [(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetPreview))], serviceProfileTags = fromList [VideoTag], serviceProfileEnabled = False}
