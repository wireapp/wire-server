{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Provider_provider where

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
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Provider
import Wire.API.Provider.Bot
import Wire.API.Provider.External
import Wire.API.Provider.Service
import Wire.API.Provider.Service.Tag
import Wire.API.User.Client.Prekey
import Wire.API.User.Identity
import Wire.API.User.Profile
testObject_Provider_provider_1 :: Provider
testObject_Provider_provider_1 = Provider {providerId = (Id (fromJust (UUID.fromString "00000008-0000-0005-0000-000700000001"))), providerName = Name {fromName = "7O\1043382\1033615\&7hI\ESC\SI9\1037813V|\1104171#"}, providerEmail = Email {emailLocal = "Rw\169526Ar", emailDomain = ",7')r"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = ":#s"}
testObject_Provider_provider_2 :: Provider
testObject_Provider_provider_2 = Provider {providerId = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000500000002"))), providerName = Name {fromName = "|du-\FSPk\990342\26220\41205\1033644V=\DEL\DEL7\NAK\61567JM\1003514\50601/\ENQW\n\1002487\28806\"z8\DC4\ETXIy\a\STX\131853\rI5C\DC3\1047698\ETB\1027103\va\21459vh\138569\187951\DC1\1001479)\21264Q\1057587<?"}, providerEmail = Email {emailLocal = "", emailDomain = "\53605/A\DC4%\ETB"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "4\994691"}
testObject_Provider_provider_3 :: Provider
testObject_Provider_provider_3 = Provider {providerId = (Id (fromJust (UUID.fromString "00000006-0000-0008-0000-000100000006"))), providerName = Name {fromName = "jE$\USl\SYN\26373q1\95896z7Mn\566\f\1000178\158213Jf\FS\21502%\165353Z\SO U\v\1100659\1009136\50411\1088301\n8f7\v(#7L\37471;\987487\1065553\98927\&0\SUB/\121029\1034023\1016015Gt2lv]_\10664SW6wTNr\1042916\3255.\32003\&7&;h\ESC\1025314\&6\1070248d\GSf!\997370ZxD\1006297\SUB\DC4\61050/Z\1064450w\USBU\NUL0E-k,`Y\42107\CAN%Q\RSo\111308RA"}, providerEmail = Email {emailLocal = "\54530\47311\98998", emailDomain = "&l\1083310`#$"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\9784\f\984492"}
testObject_Provider_provider_4 :: Provider
testObject_Provider_provider_4 = Provider {providerId = (Id (fromJust (UUID.fromString "00000006-0000-0001-0000-000400000008"))), providerName = Name {fromName = "P_\EOTSC\ENQ+<\">\95013_\1000311\DLE\SOHl\987456-oz\DC42Nc4\SUBxw\1080093Vm\4508\17024ioi\SUB6\1043874JP\19100\ENQ&X\f\n\39935`\ENQ;M\37716\EMWFB=k\FS\36089\165066\ESCyE\1025024\1069168Z\ETX\ETX\1026166\ETX\1067331\1066401C8:`(\47093;<M\nW\CAN\54765mZ1e\43014D&k\CAN\FS\FS"}, providerEmail = Email {emailLocal = "", emailDomain = "27"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\100555e\"\100997\&7\SO"}
testObject_Provider_provider_5 :: Provider
testObject_Provider_provider_5 = Provider {providerId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))), providerName = Name {fromName = "\147206,}KWN\1038621\1045668DY\161587^*%\EM\110716\SOHn[\SYN\DC3\167827\SYN\168311\574\1026337\157251c\181272\29870${}dE(\158097[jN\165851d=R$\153980\a\SI^`G\SOHt\1061329\DC1\1070665\1099272W=\FS<\70724!4XHU\EM\n3UnN\1078876\FS:\178237T\EMi@#-J\SYN/\ESCq\128525{naWK]\152826\SOR\1032616N{(\ESC^\61988,;pqR+,J\DEL\34841v\ESCy\"X~\EMKk"}, providerEmail = Email {emailLocal = "wSw\165072\DEL\1064137", emailDomain = "\1053741$L\98441IH"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "b\191044;\SI"}
testObject_Provider_provider_6 :: Provider
testObject_Provider_provider_6 = Provider {providerId = (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000800000008"))), providerName = Name {fromName = "\DC2\1084809\FSQ3j\188129\34634*\GSh\SYN\NUL\997705\174264\FS\bF\1049076>C\f\120184\136336\FS \EMu>\ESCK\\\NUL\78878z\37511\USWE\45836s*\DC3\DEL\138547H\SUB!`C\SUB\1099924n\STX\991713?C;\SUB!*\1087106HU\52680\127096^Ss/f\1020804\DC1(i\59085@\1059496p0\1049409-2\185099+k\989226z\1087865\ETBv\GS\995200\1053092\992280vq\STX\baW\DC4G\169080\1035622tN3.12"}, providerEmail = Email {emailLocal = "G", emailDomain = "\f"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\990701\n\SYNT\SO\134602"}
testObject_Provider_provider_7 :: Provider
testObject_Provider_provider_7 = Provider {providerId = (Id (fromJust (UUID.fromString "00000007-0000-0006-0000-000700000008"))), providerName = Name {fromName = "\49162\1010022m\f"}, providerEmail = Email {emailLocal = "$9\6645", emailDomain = "y~o\ETB"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "k\1104112\SI"}
testObject_Provider_provider_8 :: Provider
testObject_Provider_provider_8 = Provider {providerId = (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000400000004"))), providerName = Name {fromName = "X\SO?\183394\1014911lUW\171465Zs\1009446\74999\163242\a\137414\34452\128898\ACKN\"\ETB\1109491Q\1053614/~\152732/@x\1041333{\DC2\SI\GS\rYS25eo\EMhy_s\DEL\ACK<"}, providerEmail = Email {emailLocal = "\v\183871", emailDomain = ""}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\1087900KLFh"}
testObject_Provider_provider_9 :: Provider
testObject_Provider_provider_9 = Provider {providerId = (Id (fromJust (UUID.fromString "00000005-0000-0000-0000-000000000004"))), providerName = Name {fromName = "\STX7-a\DC1\5922\150355+\EOT\47573$Xt\157236:\188361&Sj\1026407\DC2\STX\"\993711\987861\SO\20804\&9z\157654)\ETB\44722G\170008d\135698[\74025\1082692xRt\36437\EM\US\1042242&] \1012957)?%=\nK\DC3\1109123\1042229\v\1047718\&3\1045659h\US\27365y\n\34870\&9%E\21796L\917850`Co\1006295\DC2Q\1020993\1073197D.l\t\n>\1058951\US\SUB9\SI9@\DC2e3\1007481\59515\50487C1t\1095870\1099415r\NULv&\1046207E\r\DC3f#\a3\173144Wx\1083924D\EMB\SUB"}, providerEmail = Email {emailLocal = "{Qy\RS", emailDomain = "\a\USB'd"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "U9RD\14561"}
testObject_Provider_provider_10 :: Provider
testObject_Provider_provider_10 = Provider {providerId = (Id (fromJust (UUID.fromString "00000006-0000-0001-0000-000000000007"))), providerName = Name {fromName = "@\185342\1082377t4\150686<\2360fz\ACK(\DC2(C.\1097243\1006684L\n\23736& 5P0\CAN7\ETX\\\DC2c\1059274B1\45446JU#\72148\&5X\63074\990995.\1064433\&5\985763G R|\1032875\ACK=\SUB\1089412\SO\120533\1058388\DEL\145172l\32546!\US\1028234\186763\60501\USv\f\1034025\36876\987318\148346+F\1113743z\SOH\1074735\1070776\17905\&5ie=\131476\1020978lM\ACKs\1063292N\1104808b\EM\6931AAX@\SYNH\1027847\DC3\\4\1081635"}, providerEmail = Email {emailLocal = "\58581&*", emailDomain = ""}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\1108677?0\f\SYN"}
testObject_Provider_provider_11 :: Provider
testObject_Provider_provider_11 = Provider {providerId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000300000002"))), providerName = Name {fromName = "e\181500f\1088073\&1\GSTl\EM\829BA\96729z\NUL/M^\n|\NUL\983450\GS\SI\41620\SO{+EL\45489\t\ala*\CAN\171668\57705"}, providerEmail = Email {emailLocal = "\38070\1098765\DC2", emailDomain = "_#"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "z\1034848`"}
testObject_Provider_provider_12 :: Provider
testObject_Provider_provider_12 = Provider {providerId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000004"))), providerName = Name {fromName = "\7072\1105602@hj8\b\42527XBg\146747\&8q\1020596RT)\137005\94627$u\DC1\1069750h\34737}^]\"\DC1wz\1098531G\147607ndqM>ge`\179629l\992284\46859\DLE"}, providerEmail = Email {emailLocal = "\EM}\13677'FX", emailDomain = ""}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\US"}
testObject_Provider_provider_13 :: Provider
testObject_Provider_provider_13 = Provider {providerId = (Id (fromJust (UUID.fromString "00000007-0000-0008-0000-000600000006"))), providerName = Name {fromName = "eg\21666$k\rQDy|\144062h:B^\133385Pho\176029\&5\174005Z\992460h\US\160832x\DC4q+\SO\DC4aw%h\SI\1047741/S\40116\&8u\164622$y\nQ\DLE-eP\RS~P\RS\ESC\160682nQ&T\a\1039355n:mi>\1014839\1034775EQm\1024785\DC4\51398\1036891\&3S\144680t1\DC23.k^%{\1006786W\35210l\DC2qQzz\1017872V\\\SUB\SYN"}, providerEmail = Email {emailLocal = "", emailDomain = "\152208\132461"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = ""}
testObject_Provider_provider_14 :: Provider
testObject_Provider_provider_14 = Provider {providerId = (Id (fromJust (UUID.fromString "00000007-0000-0002-0000-000600000001"))), providerName = Name {fromName = "\4426`\48407\&8|R~'^V\SYNR\US}]p"}, providerEmail = Email {emailLocal = "\ETBE}", emailDomain = ""}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = ">\174204f"}
testObject_Provider_provider_15 :: Provider
testObject_Provider_provider_15 = Provider {providerId = (Id (fromJust (UUID.fromString "00000007-0000-0003-0000-000800000001"))), providerName = Name {fromName = "kJ\DLE\NULtj1Vs\DC3)BPKKU/\27569\74277\60844em\ETB/\US\SYN"}, providerEmail = Email {emailLocal = "\11134", emailDomain = "\157148"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "Y\"\1029970\ETX"}
testObject_Provider_provider_16 :: Provider
testObject_Provider_provider_16 = Provider {providerId = (Id (fromJust (UUID.fromString "00000006-0000-0005-0000-000500000000"))), providerName = Name {fromName = "yL\27906/Fv"}, providerEmail = Email {emailLocal = "", emailDomain = "p&t\1047073g"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\EM\139887\60156"}
testObject_Provider_provider_17 :: Provider
testObject_Provider_provider_17 = Provider {providerId = (Id (fromJust (UUID.fromString "00000005-0000-0005-0000-000000000002"))), providerName = Name {fromName = "*g\1049358=hM\1038961U,!\183329\1079950\DC4\983972|\1026715\986563\f5\STXi\95074fiV\STXNJ\1017450\1065763\29127Q\b0\a\DLEBc\1095652W\1066184\ENQ\ESCl/e:79s\25719vs2X\26782\1013676w\1003684\1056752\SYNHt\997981<\41879\74804iv\1091781RY\DC4VIh.\1061996x\DC3B\164362\997028\1035744Uuw(\99195y\ETXs\1029174\172710\r=\36620\SYN\DC4\GS\60363\1035783>^h\47998\11249\162522\FS[\155784+J\185290\GS\ACKZ\EOT1lWA\1042342"}, providerEmail = Email {emailLocal = "\991353\53206r", emailDomain = ""}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\RS"}
testObject_Provider_provider_18 :: Provider
testObject_Provider_provider_18 = Provider {providerId = (Id (fromJust (UUID.fromString "00000005-0000-0007-0000-000700000008"))), providerName = Name {fromName = "A:laO\EOT\997564\DEL\27792\&5 Nq\1102089r'\1026605FA\GS\3271L\37018\CANIm8"}, providerEmail = Email {emailLocal = "uZ\ENQ#I", emailDomain = ""}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "5\55174"}
testObject_Provider_provider_19 :: Provider
testObject_Provider_provider_19 = Provider {providerId = (Id (fromJust (UUID.fromString "00000006-0000-0007-0000-000300000007"))), providerName = Name {fromName = "\ESC\a4\187144N\1078831V`8(\1099460\1013347\DC3_Oz"}, providerEmail = Email {emailLocal = "3RY\174580", emailDomain = "<1{"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "r\US\"\987328\ESCD"}
testObject_Provider_provider_20 :: Provider
testObject_Provider_provider_20 = Provider {providerId = (Id (fromJust (UUID.fromString "00000004-0000-0007-0000-000100000008"))), providerName = Name {fromName = "\1103185\STX\ACK2\nv\CAN\r\44628\ad\20578\163365\1012894\SI\83300^#S{\SI{\181471P\1096508\1034956OYJP\ACK2\nJ\174791\1016756\DC4rJ\19967\&2\1013093[he\SO\SUB{\33590\r\DLE\998988:\1045628G\ETX\EOT- %\160867w@\ETX\40343\b\156920b\46294i#ej\34341g\ESC\DC2Y?bMG\1098030l"}, providerEmail = Email {emailLocal = "}\64305\134023o\29236", emailDomain = "\1001943"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = ")yT\74782M"}
