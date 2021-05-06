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
testObject_PasswordReset_provider_1 = PasswordReset {nprEmail = Email {emailLocal = "\1103931uZ2v4E&]gc|\1084774\GSA=A\STX\10112m\a~\188150<,\SYN\SI\ACK/", emailDomain = "T"}}
testObject_PasswordReset_provider_2 :: PasswordReset
testObject_PasswordReset_provider_2 = PasswordReset {nprEmail = Email {emailLocal = "i\44278E\USa\189260wK54\1026111\149114\38900\21800\\\"\1102365c-'\38346\&6}y\1037669\CAN\FSOp\1093478", emailDomain = "\DC3\"Y{\DC2_#'v\58646!PO\ETBh\f"}}
testObject_PasswordReset_provider_3 :: PasswordReset
testObject_PasswordReset_provider_3 = PasswordReset {nprEmail = Email {emailLocal = "'oU\RS\bRLU(\n\1038843Ap:9\fdvO", emailDomain = "W"}}
testObject_PasswordReset_provider_4 :: PasswordReset
testObject_PasswordReset_provider_4 = PasswordReset {nprEmail = Email {emailLocal = "ZN\137831\40905om\ETBk\EME", emailDomain = "\a"}}
testObject_PasswordReset_provider_5 :: PasswordReset
testObject_PasswordReset_provider_5 = PasswordReset {nprEmail = Email {emailLocal = "9p\ESC^\178438\a#\ACK'\162628\133575\tu\1002249|T", emailDomain = "\EOTw\1019910n(B\146373[\f\NUL\110988%\129082&"}}
testObject_PasswordReset_provider_6 :: PasswordReset
testObject_PasswordReset_provider_6 = PasswordReset {nprEmail = Email {emailLocal = "\ENQ\CAN\CAN\1037106-A\1036309wQq3)$z\1105479^]\121335\EOT\DEL\147769\EOT", emailDomain = "_6\SYN:"}}
testObject_PasswordReset_provider_7 :: PasswordReset
testObject_PasswordReset_provider_7 = PasswordReset {nprEmail = Email {emailLocal = "tGm\DC1\1008657\155756\125079So", emailDomain = ""}}
testObject_PasswordReset_provider_8 :: PasswordReset
testObject_PasswordReset_provider_8 = PasswordReset {nprEmail = Email {emailLocal = "Cz\FS^2/\r;?\1021286\7546\STX?\1039829", emailDomain = "+L!\1048225\SYN\ETB\23061z\990853\t\1097252g\1043186\v\DLE\"\181393\1011874FA[5 !F\ESC\989055U\1018871"}}
testObject_PasswordReset_provider_9 :: PasswordReset
testObject_PasswordReset_provider_9 = PasswordReset {nprEmail = Email {emailLocal = "#\b\1059647\1052123\1084671\179288>\1094228\169888\1107180\DLE\999959\&5E\US\1026796! \1107979r\1066810\141955\a", emailDomain = "y\DC3\1010719\DLE\1033545\138507"}}
testObject_PasswordReset_provider_10 :: PasswordReset
testObject_PasswordReset_provider_10 = PasswordReset {nprEmail = Email {emailLocal = "\DC4\b\ETXE\175911\1075513\ACKq4E\917562SA\US6FG_O\CAN", emailDomain = "\1062203|}\FS\1094781R\STXb}9\14228\141490\1093418)\17078"}}
testObject_PasswordReset_provider_11 :: PasswordReset
testObject_PasswordReset_provider_11 = PasswordReset {nprEmail = Email {emailLocal = "\1048885|\DC2\1036190\131930\1000595[a\SIFY\48290\133993$x=#j\1083808", emailDomain = "\EM"}}
testObject_PasswordReset_provider_12 :: PasswordReset
testObject_PasswordReset_provider_12 = PasswordReset {nprEmail = Email {emailLocal = "\28212\&3b\27116\&1\NUL\",^\49685\1041018", emailDomain = "\33820\r\992812:\171366||\ETX?t\1029784\&0\1011482\27469Sc\182132\988065f\ENQ>\ENQ]"}}
testObject_PasswordReset_provider_13 :: PasswordReset
testObject_PasswordReset_provider_13 = PasswordReset {nprEmail = Email {emailLocal = "'E\157914\&8\EM\127367\CAN\126647?'\SO\19349XW\1058191\EOTU", emailDomain = "."}}
testObject_PasswordReset_provider_14 :: PasswordReset
testObject_PasswordReset_provider_14 = PasswordReset {nprEmail = Email {emailLocal = "\7137\RS\1054860abT\138078p&\163239^t\tSR", emailDomain = "\62490&\1105881i\ETX?Fn:\61354"}}
testObject_PasswordReset_provider_15 :: PasswordReset
testObject_PasswordReset_provider_15 = PasswordReset {nprEmail = Email {emailLocal = "[\b6\993885X\1021640\SI\169964\DC4*Pr\1067840\STXi", emailDomain = ""}}
testObject_PasswordReset_provider_16 :: PasswordReset
testObject_PasswordReset_provider_16 = PasswordReset {nprEmail = Email {emailLocal = "\b\54672\&5\1103374\179323)\DC1\DELB#\176656>\DC2\ACK\NAK\DC3\990960\f\14213'~no\"i\SOHT", emailDomain = "\16146a|"}}
testObject_PasswordReset_provider_17 :: PasswordReset
testObject_PasswordReset_provider_17 = PasswordReset {nprEmail = Email {emailLocal = "s", emailDomain = "uQ\EOTf"}}
testObject_PasswordReset_provider_18 :: PasswordReset
testObject_PasswordReset_provider_18 = PasswordReset {nprEmail = Email {emailLocal = "Yuv).I\SO\17384$L\992746\&0>", emailDomain = "F\nXb\1056723\SI\1068061k\1104075{\48485P|e\1035328^;\1006508\DLE\1089661X,N\STX\1091938\1089614\ETB"}}
testObject_PasswordReset_provider_19 :: PasswordReset
testObject_PasswordReset_provider_19 = PasswordReset {nprEmail = Email {emailLocal = "\98345\187974\174208", emailDomain = "8\1033258r"}}
testObject_PasswordReset_provider_20 :: PasswordReset
testObject_PasswordReset_provider_20 = PasswordReset {nprEmail = Email {emailLocal = "\1110466\168684yq\NAKx\FS[\DC4\US", emailDomain = "RG-#\ESC\3612#RS\ETX$d9\ACKu\n\144156{\rl"}}
