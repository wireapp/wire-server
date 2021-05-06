{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UpdateProvider_provider where

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
testObject_UpdateProvider_provider_1 :: UpdateProvider
testObject_UpdateProvider_provider_1 = UpdateProvider {updateProviderName = Just (Name {fromName = "(Gk\991886\38539)g\STX#\DC41\1063140W\1005908s|\NAKf\DC3\EM\"A\f\SUB\190452@/-\97645\"7\185322^\SICPh\rd7n4-v\993404g\ETXz\20860\992562\987423F\1066715i\1067209q\1014919\NAK1&\n0zGn\428q\97206\SOH;]\1068306J\ESC6kr\SOH"}), updateProviderUrl = Nothing, updateProviderDescr = Nothing}
testObject_UpdateProvider_provider_2 :: UpdateProvider
testObject_UpdateProvider_provider_2 = UpdateProvider {updateProviderName = Just (Name {fromName = "\142288@`\\{Z^\110651\987084\NAK\SOH\1013805\b\58748\184691mq\ACK\1065147\180468|\a\1109390=~\1086929\&9}\SO];#.ep?\910;;\1028838p*Y\154576E9mp\\C]&ZGy`\1039773\STXx\1071276\150646\1034067X\993965:-t[5.\159502\"x2.w\25780q\CANof/[\1080075\994389D5gvX\DLE79nz1\1050895\21113\168504(\1016575\150485\&6yY$J5|\t6\DLEx\1025005\1091007\188950\7611fD9{}"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just ">JX\156246\&2.\CAN\ax"}
testObject_UpdateProvider_provider_3 :: UpdateProvider
testObject_UpdateProvider_provider_3 = UpdateProvider {updateProviderName = Just (Name {fromName = "\1075485{~]\166693\US\54014+J>*\ESCS\DC2(\ENQ\GSN\1089697Ho\1092111\&5\\<\EOTO\t\1052017e\133425T\b\DC1\NAK\1112886\1052617\148858\b\ETB$\DC4\\\SOH\998639\1092900\165239Em\tv)\ESC\DC1\48945:T\FSz\DC4/ 7T!\US\DC3MdZI\100021\149997oDe\1045783\ENQ\DC1L_K^\US3%\42950\1000634\ACK\1088858tNC\64019/\t"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "n\157784\SI\1083600\DC2_U"}
testObject_UpdateProvider_provider_4 :: UpdateProvider
testObject_UpdateProvider_provider_4 = UpdateProvider {updateProviderName = Just (Name {fromName = "c=\CAN \100562\DLE\71051J\1056475GX:il$\DC1\990867\&2sk^\189728\EOTMltL-\DC4j8\ENQ\29515\DC1\24884\DC2\1048902njs`XZ*\ETXF-\\o(,x\\\b:\SI\7797s\69433{E\1044262\SYN%H\1053351~W2\1052876v\SOH$a\r's@1O\111091+gVz!\rVr\14669It\1104271a\17268\1027994%\DLE!5\SYN\1007427)N\181342\&0d\1010753xPgAm\996747:\55015"}), updateProviderUrl = Nothing, updateProviderDescr = Nothing}
testObject_UpdateProvider_provider_5 :: UpdateProvider
testObject_UpdateProvider_provider_5 = UpdateProvider {updateProviderName = Just (Name {fromName = "\tG`\1074412\US\STX<\"B\152968\1067076\1110828\r)B\ESCU6@1\1032189\1053055\1101159UR\NAK\EOT\RS%\"~\DC4'vy2\11763uq\1105875)H\1002574:m\96627\RS\RSU\44062\r{\t!L\SOH)\DC1\47082\53575>S\\s\1095184lJ\vY\1065149\44362\ETB\SOg\1108843^7UU"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\159319\1028553!\n~\52709$V\1033908"}
testObject_UpdateProvider_provider_6 :: UpdateProvider
testObject_UpdateProvider_provider_6 = UpdateProvider {updateProviderName = Just (Name {fromName = "\167723#\NUL\USw\NAKm\fZ\RSC\DC2\RS^TwG.f\997394\n\183968\160968\1087478l\181210,Wn\96959\&5\GSw\DC4\SI#\100137.m\US5"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\161466\&7\1031715*\DC2q\NAK"}
testObject_UpdateProvider_provider_7 :: UpdateProvider
testObject_UpdateProvider_provider_7 = UpdateProvider {updateProviderName = Nothing, updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "0d\1089780.G 6"}
testObject_UpdateProvider_provider_8 :: UpdateProvider
testObject_UpdateProvider_provider_8 = UpdateProvider {updateProviderName = Nothing, updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Nothing}
testObject_UpdateProvider_provider_9 :: UpdateProvider
testObject_UpdateProvider_provider_9 = UpdateProvider {updateProviderName = Nothing, updateProviderUrl = Nothing, updateProviderDescr = Just "kJ\138613\&6"}
testObject_UpdateProvider_provider_10 :: UpdateProvider
testObject_UpdateProvider_provider_10 = UpdateProvider {updateProviderName = Just (Name {fromName = "\GSkfVC\ACK\\Kv\174262"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just ""}
testObject_UpdateProvider_provider_11 :: UpdateProvider
testObject_UpdateProvider_provider_11 = UpdateProvider {updateProviderName = Just (Name {fromName = "I\148897TV\98104p\FS\1028980\152257\1082973\DC1s,U]\1093869\159350+\SO\1017445\995014)\151723umQD\991557^P\174053\25011\CAN,mb\997970t\t\EM\1055911\SUB/J!\1051267on'A^s\188364\&8gS\ETX\1081097#+s\1099740\1042873\DC2\r\142849F\190709\1077331\&7v\\;\23035\NULO\EM\DC4\15635$ksi\995060\1088661\\$D\a\37370\"<\ACK\1085156\nzH\62778V\1069045\1101132\1020924\f"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Nothing}
testObject_UpdateProvider_provider_12 :: UpdateProvider
testObject_UpdateProvider_provider_12 = UpdateProvider {updateProviderName = Nothing, updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "i"}
testObject_UpdateProvider_provider_13 :: UpdateProvider
testObject_UpdateProvider_provider_13 = UpdateProvider {updateProviderName = Nothing, updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "W?\32792|1*\STXq"}
testObject_UpdateProvider_provider_14 :: UpdateProvider
testObject_UpdateProvider_provider_14 = UpdateProvider {updateProviderName = Just (Name {fromName = "\DLE%{N`\1027862*t\r'`|\a\NAK\166624p?}\149353\51608!v0<^P~9\175759:W\FS1\1066779p4\24440"}), updateProviderUrl = Nothing, updateProviderDescr = Just "eH\1112515R\1077306\DC4"}
testObject_UpdateProvider_provider_15 :: UpdateProvider
testObject_UpdateProvider_provider_15 = UpdateProvider {updateProviderName = Just (Name {fromName = "T\US\1063541\1017387S\1030678p\RS9-\ETXv\98324\172833eF\EOT\97707\ETX\1006992.heO \r\t\RS\GS\t\32504\1002185-\139217I\996541\SO\1075102\ETX\984960\bGM4\DC2!6\DC2\1082744\DLEr5\1014402\ETX7|\1028546\161632\b~\b\133516\&3q(\SI\SUB\1042512\SI|H\182617&o"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\SID\1018698/\42913N\1043700\98170\&0\983850"}
testObject_UpdateProvider_provider_16 :: UpdateProvider
testObject_UpdateProvider_provider_16 = UpdateProvider {updateProviderName = Just (Name {fromName = "zg\37057\43989\57818@7O\DC4\36110\1075001\1020870\176649i\DC4\1027772=D\128539\SYN0\145315s\ESC8\22750\DC4^\32744\195075\138642?\DC3\17690`P'(7\1078185\72108kI\137235\b+!H\989675-V/Y#\a\167172\SI\DLE@K\1110780lzZ\180318\1032463\999220y:\aX\nT\r-\ESC\180495{\1068528\r\1007830\SUB]\ACK\46383S\147687\ETBvHC\58004\ETX\1043479\1082690\bNF}L\SYN`4\31110\168922G_j\f\CAN\30966\1021087\SOHZT5K"}), updateProviderUrl = Nothing, updateProviderDescr = Nothing}
testObject_UpdateProvider_provider_17 :: UpdateProvider
testObject_UpdateProvider_provider_17 = UpdateProvider {updateProviderName = Just (Name {fromName = "\1023348\&92,\1035658\133547\SUB\CAN7_\DLE\177736Qai\1032997F-2\17191{\ESC%\70046\aE^\\\ESC\151128\DC1Q\a\1075826A;Hl\1034082$\1098973!@\DC2\999460\1018272\1100884\1038175iP\29541\1003587a\CANi\CAN\DEL\SOH@\1006513\&3\ETBf3\60696r_YL\1032295\1102736?\GS$\994559y#\1012577TlrHz?x\ENQ\RSaFq]\nd@\DC4\1110\1048562\1007525\n\1088106b\tz\1025178\1088845f\1052422;?\25132\DC2Nw;\DC2\n\152286"}), updateProviderUrl = Nothing, updateProviderDescr = Nothing}
testObject_UpdateProvider_provider_18 :: UpdateProvider
testObject_UpdateProvider_provider_18 = UpdateProvider {updateProviderName = Just (Name {fromName = "jh\33227~\vW\1038971\10984"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\GSm"}
testObject_UpdateProvider_provider_19 :: UpdateProvider
testObject_UpdateProvider_provider_19 = UpdateProvider {updateProviderName = Just (Name {fromName = "\EOT\FS.\DEL&\999030\DEL\DC3\"\ESC\ACKW=\37524G(`f\187569\US}+j\1090561\1053846\166618NN\DLE\1039251voP%rP\ETB&\78681\1004681.\ESC:"}), updateProviderUrl = Nothing, updateProviderDescr = Just "\STXF\174029%\NULC"}
testObject_UpdateProvider_provider_20 :: UpdateProvider
testObject_UpdateProvider_provider_20 = UpdateProvider {updateProviderName = Just (Name {fromName = "\994164\GSc\170820K)\1085436|\1091981B\ETX\SIj1C\US})`aj>C\49867\175448i0\1100351&1\1095541i\152830L\DC4\38493\ETB\153759\DC2\USV\170342\42114\1019301yTX}4udMi*}(uT\1014909"}), updateProviderUrl = Nothing, updateProviderDescr = Just ""}
