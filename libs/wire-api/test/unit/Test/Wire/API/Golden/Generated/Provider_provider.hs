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
testObject_Provider_provider_1 = Provider {providerId = (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000500000001"))), providerName = Name {fromName = "5n\STXp\167272\&0\v\SIEa\DLE\EOT\SO\CAN\61085`g\1011523|j\nR.f\fO}\1071333(\DEL\30570\63237z\1100573W\"\NAK\29015\DC2IZ\53772\1080040Q\997755\99812\GS\RS\1103418\&6\45630\FS\STX\1087829\1110151[\DC1o/\1095715\SO=\172876T9\1079416\1101768\38868i\FS=KH\1071321Pw\1017119\SYNFHQ3"}, providerEmail = Email {emailLocal = "K\999232\STXi", emailDomain = ""}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\b~-8\\."}
testObject_Provider_provider_2 :: Provider
testObject_Provider_provider_2 = Provider {providerId = (Id (fromJust (UUID.fromString "00000007-0000-0000-0000-000700000007"))), providerName = Name {fromName = "zlL]\1059417\DC1q5\29772\991923\ESC\SO\SOH<\DEL\66877\1037520\142825C(zKx\1087613SA\DC4\"]r\FS\1022533s\t"}, providerEmail = Email {emailLocal = "", emailDomain = "p\5749:\SYN"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = ""}
testObject_Provider_provider_3 :: Provider
testObject_Provider_provider_3 = Provider {providerId = (Id (fromJust (UUID.fromString "00000006-0000-0001-0000-000600000005"))), providerName = Name {fromName = "0\999826\SUBEk]\1010450YfUY.Iz\1082895\1016494o\DC2g\ESC\68338\154626*\CAN@R\10717\ETXCN\17817\1046456\1114091D w\ETXn/\EM\DC3\a\USMa\1103982UD*\td6O"}, providerEmail = Email {emailLocal = "D}}", emailDomain = "AR\US\31824"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "7\98562LwK\DC4"}
testObject_Provider_provider_4 :: Provider
testObject_Provider_provider_4 = Provider {providerId = (Id (fromJust (UUID.fromString "00000008-0000-0004-0000-000000000001"))), providerName = Name {fromName = "\14818\r7eO\164187\189903\\z@\1012100B\DLE<\132766\63669xs\CAN\1103154\162798e\33367\27220\NUL[\r\994131E>F'M+O0TA\129135b\164107\35483^<\180237\NUL{;YSCT:\SYN\7295\1054050t\a\1009614u\ETX|;_4ME\163409*@C\"l\1067229\SOH'H\63661-Y}\54965CT<\ACK\DLE\ESCL\r\137658ql%t\vu4T&.=|"}, providerEmail = Email {emailLocal = "\t7\1089537\b", emailDomain = "`"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\159808\&8"}
testObject_Provider_provider_5 :: Provider
testObject_Provider_provider_5 = Provider {providerId = (Id (fromJust (UUID.fromString "00000004-0000-0008-0000-000800000006"))), providerName = Name {fromName = "#\SUBDS:6\154932\&5}@F\181703ZI\"\SO\ah0\1022180\1042359\ESC@c\187820\7863\SOH4f)\94794F~\RS\133832\1085157x\1093375J\154136\100555?^m<(\150690#\SI|\990595\EOT\NUL\1002031V\150527\&5}\SOH~,7\95193\DEL\CAN\1086587\ENQt+\1107492X>\"Tq)T\25761h_ei9\157099?i\1057594\6212t\NAK\DC32*\EM\1055181;K_\148722M&fj{0\144278\EM\1038515lr>\f(\EOT(['4Y[\a"}, providerEmail = Email {emailLocal = "", emailDomain = "\24691j\186152#\151224"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\US"}
