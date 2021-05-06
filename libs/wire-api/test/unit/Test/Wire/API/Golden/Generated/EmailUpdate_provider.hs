{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.EmailUpdate_provider where

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
testObject_EmailUpdate_provider_1 :: EmailUpdate
testObject_EmailUpdate_provider_1 = EmailUpdate {euEmail = Email {emailLocal = "rOqA\SOB#\EM\6074\v\1071978q\97248\1099759]CE\1041500P\SO\SUB\DC4R\174131k$\1095957", emailDomain = "\SOH"}}
testObject_EmailUpdate_provider_2 :: EmailUpdate
testObject_EmailUpdate_provider_2 = EmailUpdate {euEmail = Email {emailLocal = "_Q2\181062\34571\1037254/n \SO\187079p\1003767\1090984\SOl,#{/q\64911b(<k\63549\EMX", emailDomain = "{!\ACKn`\1077288Y\1018902\&2\993339|\t\DELCy5\a-<\1005532"}}
testObject_EmailUpdate_provider_3 :: EmailUpdate
testObject_EmailUpdate_provider_3 = EmailUpdate {euEmail = Email {emailLocal = "\SYNEb\1031971\131742z\1011062;WH\67228{_\1065383\DELMl\tI\bu", emailDomain = "\38767\1109410-t\15733$DrYtPC\1051078\&8\DC4N\1092194"}}
testObject_EmailUpdate_provider_4 :: EmailUpdate
testObject_EmailUpdate_provider_4 = EmailUpdate {euEmail = Email {emailLocal = "\trM\1015466\RSt\ETB\SO\1113115\NAK\1101370M", emailDomain = "!\GS\"\134471\&91$e\RSHP\1049166\1106808Fz\SYN\a5\r\39371\GS\RS\fi"}}
testObject_EmailUpdate_provider_5 :: EmailUpdate
testObject_EmailUpdate_provider_5 = EmailUpdate {euEmail = Email {emailLocal = ":>\60191aX", emailDomain = "\ETX\DC1G\184861\43121g"}}
testObject_EmailUpdate_provider_6 :: EmailUpdate
testObject_EmailUpdate_provider_6 = EmailUpdate {euEmail = Email {emailLocal = "yl\1033530\SYN\ESC}\ENQ\66223\154717\1011141&", emailDomain = "0\1013338\tC\STX\53883X\CAN\110764\1031106;x\1016622_p\39694"}}
testObject_EmailUpdate_provider_7 :: EmailUpdate
testObject_EmailUpdate_provider_7 = EmailUpdate {euEmail = Email {emailLocal = "_phgI\US\1017046y\ETBu\SUBM\172243(\10324\&3&", emailDomain = "t\1064480\STX&uO\1111813\182131\DC2\SOH\1094208T~x\1072004\t("}}
testObject_EmailUpdate_provider_8 :: EmailUpdate
testObject_EmailUpdate_provider_8 = EmailUpdate {euEmail = Email {emailLocal = "Z\1003813h\1113927", emailDomain = "\"\138616[\EM5DCFSC\137764C4B\SIa\990555\1049642Ax\DC2\1031168e;("}}
testObject_EmailUpdate_provider_9 :: EmailUpdate
testObject_EmailUpdate_provider_9 = EmailUpdate {euEmail = Email {emailLocal = "\ENQZl\1107286TC\ENQ\ESC>\1053024U\27716\SUB\95484\169900Hn\1064388\179272\&4\1021560'(u\NAK\1011184=", emailDomain = "^Ms\1018442\1096072\SUBLt~:]\DC43pxL?=-)\DC2\SUBO\991177\\s\185213\SIz"}}
testObject_EmailUpdate_provider_10 :: EmailUpdate
testObject_EmailUpdate_provider_10 = EmailUpdate {euEmail = Email {emailLocal = "2\FS\29960+\65335'\n-/m9", emailDomain = "Z.nI\38940+E\US\GS\DC2p\1111464-=5qe\19302"}}
testObject_EmailUpdate_provider_11 :: EmailUpdate
testObject_EmailUpdate_provider_11 = EmailUpdate {euEmail = Email {emailLocal = "\1102111\GS\35501\DLEHW\1074395", emailDomain = "wwi\1004609i\2267\168694b=C.+Y"}}
testObject_EmailUpdate_provider_12 :: EmailUpdate
testObject_EmailUpdate_provider_12 = EmailUpdate {euEmail = Email {emailLocal = "\DC4\1108061*>", emailDomain = "_cAd\f\17256\155862\&3YUy\ENQEVf\SOQ\EOT\1041153\n`"}}
testObject_EmailUpdate_provider_13 :: EmailUpdate
testObject_EmailUpdate_provider_13 = EmailUpdate {euEmail = Email {emailLocal = "X3", emailDomain = "!\163696\t w\996224\DC4F\DEL\47707RT\DC17"}}
testObject_EmailUpdate_provider_14 :: EmailUpdate
testObject_EmailUpdate_provider_14 = EmailUpdate {euEmail = Email {emailLocal = "\52743Nw,F\1020601\SOH\6596\US]MB\ETB/\143426,\1065000!\13993\&5", emailDomain = "&\1022064"}}
testObject_EmailUpdate_provider_15 :: EmailUpdate
testObject_EmailUpdate_provider_15 = EmailUpdate {euEmail = Email {emailLocal = "y<\144339", emailDomain = "vT\16714\61114\38815\DC1\143330\DC4T_N._\EM\FS1+\32838b\147180"}}
testObject_EmailUpdate_provider_16 :: EmailUpdate
testObject_EmailUpdate_provider_16 = EmailUpdate {euEmail = Email {emailLocal = "\73085\983807u\1073394r\ETX\f(|\"iZ^BiD\1084556\48012\SI", emailDomain = "N\1021860[b\15448\CANz\128475Kj\SI"}}
testObject_EmailUpdate_provider_17 :: EmailUpdate
testObject_EmailUpdate_provider_17 = EmailUpdate {euEmail = Email {emailLocal = "z\SYNx", emailDomain = ";\155904\99718J"}}
testObject_EmailUpdate_provider_18 :: EmailUpdate
testObject_EmailUpdate_provider_18 = EmailUpdate {euEmail = Email {emailLocal = "%0tG\1030111+4\188162\v?\73020\50450#G\53891+ 3", emailDomain = "\1079687\20051u\NUL\51278"}}
testObject_EmailUpdate_provider_19 :: EmailUpdate
testObject_EmailUpdate_provider_19 = EmailUpdate {euEmail = Email {emailLocal = "\183047\ACK[\ETBR\a\EM3\EMp\RS\184231\&2\n\8641y8ju,\405\CANa\DC2", emailDomain = "y\US\a\50078b\1105817\EOT\DC3\DC2\t\1068608\CANm\NAK\1089653G"}}
testObject_EmailUpdate_provider_20 :: EmailUpdate
testObject_EmailUpdate_provider_20 = EmailUpdate {euEmail = Email {emailLocal = "N2]\SUBIH)\DC22^T4\20513a\63658\&6\14268{P\1058408/\1074742\171430", emailDomain = "Zh\FSm\175756W"}}
