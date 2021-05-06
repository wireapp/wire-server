{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.PasswordReset_provider where

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
testObject_PasswordReset_provider_1 :: PasswordReset
testObject_PasswordReset_provider_1 = PasswordReset {nprEmail = Email {emailLocal = "sk\1042062=\46159\21199\24081", emailDomain = "\DC3\194644\ENQ\1047874\184976\9519\DC1jG<Kp\1066607jP\119570N\"J.\"*\44579\&1R]"}}
testObject_PasswordReset_provider_2 :: PasswordReset
testObject_PasswordReset_provider_2 = PasswordReset {nprEmail = Email {emailLocal = "Qq][\1062755L4tgR\STX\NUL\f\98506\18491\ESC|\994515&\1094328d\STX\SO", emailDomain = "A\EOT|\144053?AVX\171687CG\1091363=2B\DEL\27600\1003283M\f4]\\\984457\1014386XI\\\148846"}}
testObject_PasswordReset_provider_3 :: PasswordReset
testObject_PasswordReset_provider_3 = PasswordReset {nprEmail = Email {emailLocal = "-ZN\989419\171636\3672v\DC3\145418$WrWy\1029435\&1\SYN", emailDomain = "m\ETX%"}}
testObject_PasswordReset_provider_4 :: PasswordReset
testObject_PasswordReset_provider_4 = PasswordReset {nprEmail = Email {emailLocal = "\ENQ\997022\997374\FS\t\43829O\163038\RS\33588s!U\CAN\FS\1029763\3611$", emailDomain = "\f\1060087L\1022171\USH?]%\65837*di\1019281\31989}c><\186418\US\48599v\ENQ"}}
testObject_PasswordReset_provider_5 :: PasswordReset
testObject_PasswordReset_provider_5 = PasswordReset {nprEmail = Email {emailLocal = "\a", emailDomain = "\ETX\DC1\\(P\"I<>\55166\DC1\SO\"\DC2\1102806\b\96709t\DC1?\v\GS"}}
testObject_PasswordReset_provider_6 :: PasswordReset
testObject_PasswordReset_provider_6 = PasswordReset {nprEmail = Email {emailLocal = "\4146v!q\57729\STX6", emailDomain = "\STXW\EOTs>n\STX\DC1\150256qod\r&\FS\\\FSJ\179457\&8\1057360\"i\1079344\1055995k"}}
testObject_PasswordReset_provider_7 :: PasswordReset
testObject_PasswordReset_provider_7 = PasswordReset {nprEmail = Email {emailLocal = "<\154336\avWW\ACK9\29247&", emailDomain = "(\vt\162798\984657}\1063514QB\1080697TW\1056254\"\44478&\1013410\1047860\ESCD"}}
testObject_PasswordReset_provider_8 :: PasswordReset
testObject_PasswordReset_provider_8 = PasswordReset {nprEmail = Email {emailLocal = "\1061897PFft\1028970\ESC", emailDomain = "M._\17230RDd4\ETB\fn\ETB^J$s+\25677J\SOHU\72883\1105015\EOT\1036600\156343z"}}
testObject_PasswordReset_provider_9 :: PasswordReset
testObject_PasswordReset_provider_9 = PasswordReset {nprEmail = Email {emailLocal = "q`", emailDomain = "5>nCZ\SYN\EOT}\38065\ENQ\129065\53722-?(\1109103\EM"}}
testObject_PasswordReset_provider_10 :: PasswordReset
testObject_PasswordReset_provider_10 = PasswordReset {nprEmail = Email {emailLocal = "{l\DC1:", emailDomain = "[!%\838\1096095\a\ENQL;\1048030\SO\SUB\n\RS\38006\b\47101\USu\183396^\SUB"}}
testObject_PasswordReset_provider_11 :: PasswordReset
testObject_PasswordReset_provider_11 = PasswordReset {nprEmail = Email {emailLocal = "}\n\1095644~BIw\1002905.\59881\&7D\a,\DC1{\1001669\&1", emailDomain = "\ad4\1068566oU6I\54413\SYNLg\1057792Cn\173864\1077709\ACK\1056682t\137109"}}
testObject_PasswordReset_provider_12 :: PasswordReset
testObject_PasswordReset_provider_12 = PasswordReset {nprEmail = Email {emailLocal = "\153887)r\n\DLEv?\SUBTK\1037956", emailDomain = "}<(\95549L\1000144\179527~\1015224\f\1090303hr"}}
testObject_PasswordReset_provider_13 :: PasswordReset
testObject_PasswordReset_provider_13 = PasswordReset {nprEmail = Email {emailLocal = "CA\DC3\DC4\1087806M:y\15223V\13027", emailDomain = "\ENQ\74137\37344u\5970\&3\EOTi3i\148649c=G "}}
testObject_PasswordReset_provider_14 :: PasswordReset
testObject_PasswordReset_provider_14 = PasswordReset {nprEmail = Email {emailLocal = "\EOT\1008102\1072882\194730y)w2\SO\999750`\1011985C\NAK\ENQ", emailDomain = "\"\"\1045467\181595cL1A7\33813lK!\DC1\ETB#E"}}
testObject_PasswordReset_provider_15 :: PasswordReset
testObject_PasswordReset_provider_15 = PasswordReset {nprEmail = Email {emailLocal = "\1096943/4\1044018\172231\DC2\164027\167464\NAKa#o$HO\ETB", emailDomain = "m\1111588\v\137137\139220"}}
testObject_PasswordReset_provider_16 :: PasswordReset
testObject_PasswordReset_provider_16 = PasswordReset {nprEmail = Email {emailLocal = "", emailDomain = "\59602\STX\RS\SUB\GS\SI\1003475\&7\1106075|!6d\167815N\NAK"}}
testObject_PasswordReset_provider_17 :: PasswordReset
testObject_PasswordReset_provider_17 = PasswordReset {nprEmail = Email {emailLocal = "A\55152L7\58257\SO\ESCk\1095597\1039094]q\1041010srA\\CY", emailDomain = "\DC1D\186141\11037\US\NAKV\DLE\1001692X Y"}}
testObject_PasswordReset_provider_18 :: PasswordReset
testObject_PasswordReset_provider_18 = PasswordReset {nprEmail = Email {emailLocal = "\SUBA\ETX\v\92201.WX\1082060{\ETBY\SO\1045254", emailDomain = "Jt!o\57505\1092621\165902NA"}}
testObject_PasswordReset_provider_19 :: PasswordReset
testObject_PasswordReset_provider_19 = PasswordReset {nprEmail = Email {emailLocal = "9s\985258\143263", emailDomain = "\1055612\a\tB\NAK2:\STXv \1108654\SYN4\ETB%w"}}
testObject_PasswordReset_provider_20 :: PasswordReset
testObject_PasswordReset_provider_20 = PasswordReset {nprEmail = Email {emailLocal = "\20413", emailDomain = "Ex<U~\DC3\SUB\4934\b\r\19499v9K\aIY\FSR"}}
