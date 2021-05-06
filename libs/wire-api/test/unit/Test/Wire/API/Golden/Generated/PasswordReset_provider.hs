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
testObject_PasswordReset_provider_1 = PasswordReset {nprEmail = Email {emailLocal = "\n\67263\172336 \11131\144556\1065607\DC4\GS\44309\v\143401>\988541\1020963\96436`pS7jX3\993177\b8g}?", emailDomain = "\15604\1102601\1058450\146675\GS`V\1102065:nby"}}
testObject_PasswordReset_provider_2 :: PasswordReset
testObject_PasswordReset_provider_2 = PasswordReset {nprEmail = Email {emailLocal = "\ESC\DC1", emailDomain = ""}}
testObject_PasswordReset_provider_3 :: PasswordReset
testObject_PasswordReset_provider_3 = PasswordReset {nprEmail = Email {emailLocal = "r\1100496\94798Fv&\1018381,,d0D\66280", emailDomain = "Q\SUB\SUB\FS\6059Px\STX\142544d>"}}
testObject_PasswordReset_provider_4 :: PasswordReset
testObject_PasswordReset_provider_4 = PasswordReset {nprEmail = Email {emailLocal = "", emailDomain = "smy\181375\19088\1019799N\fN\1079542+"}}
testObject_PasswordReset_provider_5 :: PasswordReset
testObject_PasswordReset_provider_5 = PasswordReset {nprEmail = Email {emailLocal = "\1067814\50113\1113347", emailDomain = ")i\140973\1104790r\7077\1033771\DEL\NUL\185126\DC4\RS\tm\n\1101642(Ec!UT\70157\1072722A\DC3"}}
testObject_PasswordReset_provider_6 :: PasswordReset
testObject_PasswordReset_provider_6 = PasswordReset {nprEmail = Email {emailLocal = "K\178488~\b>n[\SYN2\SOH", emailDomain = "\1032483;Ff_\NAKBz,G\1039967]!1\ESC\"\ACK"}}
testObject_PasswordReset_provider_7 :: PasswordReset
testObject_PasswordReset_provider_7 = PasswordReset {nprEmail = Email {emailLocal = "\111275zn\ETB\121163X\50062d~\SOH\SOH\1064331!'\1006076k\\\97096\"=Z\139059\&8\vTV]\1077163", emailDomain = "\39916|\a\DLE(O\45639\ENQm\1085403\a\RS^\GS%\149319!}"}}
testObject_PasswordReset_provider_8 :: PasswordReset
testObject_PasswordReset_provider_8 = PasswordReset {nprEmail = Email {emailLocal = "5", emailDomain = "h\EOT$\t\155631\16131\1044412\26699\155664\145478ou\DEL`\GS\SUB\DLE\EOTLyI\57497hg\52044\&9\ETB"}}
testObject_PasswordReset_provider_9 :: PasswordReset
testObject_PasswordReset_provider_9 = PasswordReset {nprEmail = Email {emailLocal = "h\983067n6N#\SUB)w\1005451\14786\14998r\133786\52184", emailDomain = "x\1029141\175091\160524Bq aT\a\DLE\1109419\&7\SO\1094172\991781\&4o1|}"}}
testObject_PasswordReset_provider_10 :: PasswordReset
testObject_PasswordReset_provider_10 = PasswordReset {nprEmail = Email {emailLocal = "\995680\21585}Pb\DC4\78037\1074501Z\17412e\148751B_\DC2\992693\1042039Wl\b2", emailDomain = ""}}
testObject_PasswordReset_provider_11 :: PasswordReset
testObject_PasswordReset_provider_11 = PasswordReset {nprEmail = Email {emailLocal = "\148959\SUBXf\1065153M\f", emailDomain = ")\v\SI\54560y_!H%\SOHe\DC2A\SYN\NULzB\FS\1010045N\12748c}\161088\1086691"}}
testObject_PasswordReset_provider_12 :: PasswordReset
testObject_PasswordReset_provider_12 = PasswordReset {nprEmail = Email {emailLocal = "j4h\995874\DC2\1078833J30\tnom\DEL\153118\153423+|\99341\&2OLa\1090679\&6Q!8L\188872", emailDomain = "(\USE5/\n\1066491rmz\154475~\SYN\1093988`8_\9923"}}
testObject_PasswordReset_provider_13 :: PasswordReset
testObject_PasswordReset_provider_13 = PasswordReset {nprEmail = Email {emailLocal = "nS\1069141\n:*(fK:(&D", emailDomain = "\USG\1078769)\19207\1067505\134687\169997F\SOHy\STXiS\1065364"}}
testObject_PasswordReset_provider_14 :: PasswordReset
testObject_PasswordReset_provider_14 = PasswordReset {nprEmail = Email {emailLocal = "", emailDomain = "D\EOTex\20455>`"}}
testObject_PasswordReset_provider_15 :: PasswordReset
testObject_PasswordReset_provider_15 = PasswordReset {nprEmail = Email {emailLocal = ")\14790\SOH-{HHd+\51059\STX", emailDomain = "U\137788'\161176\STX\1075904\ACK1\ETX;Og\1088243%m"}}
testObject_PasswordReset_provider_16 :: PasswordReset
testObject_PasswordReset_provider_16 = PasswordReset {nprEmail = Email {emailLocal = "h&\1014283\DEL,$.\1097010y\1061254y\1065209\US)f", emailDomain = "v\1108427WU\1018826["}}
testObject_PasswordReset_provider_17 :: PasswordReset
testObject_PasswordReset_provider_17 = PasswordReset {nprEmail = Email {emailLocal = "9Wn=1e", emailDomain = "{\ESCF9tZ\DELa{\NAK:T\9484\DC1"}}
testObject_PasswordReset_provider_18 :: PasswordReset
testObject_PasswordReset_provider_18 = PasswordReset {nprEmail = Email {emailLocal = "<\CAN3L5\1049008\992330\ETX \1048012\RSfu1z\RS\561", emailDomain = "\FS8\50215z|\1098375ts+\SYNDk\137350]\1006304"}}
testObject_PasswordReset_provider_19 :: PasswordReset
testObject_PasswordReset_provider_19 = PasswordReset {nprEmail = Email {emailLocal = "\DLEY\t;\1064059Dk\SO\ETBR\"[.N\42364\ETB';VY\174047w", emailDomain = "[s?\1080754P\NUL=\183180Mz\1014315E\ESC\140174\176404\1058677_\SO`\1005838"}}
testObject_PasswordReset_provider_20 :: PasswordReset
testObject_PasswordReset_provider_20 = PasswordReset {nprEmail = Email {emailLocal = "\149820S&<\54127\1114036\STX\95475nu5m\1072700X2\r\\b\62787\NULu\DC1\1107549\1020427", emailDomain = "\156322\ETX9\1003824\SOH\ETBo\b?f\DC1,\RS\DC4~Rc0\DELAn#p5?S"}}
