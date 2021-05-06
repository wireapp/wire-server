{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UserUpdate_user where

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
import Wire.API.Asset
import Wire.API.Asset.V3.Resumable
import Wire.API.Call.Config
import Wire.API.Connection
import Wire.API.Conversation
import Wire.API.Conversation.Bot
import Wire.API.Conversation.Code
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing
import Wire.API.CustomBackend
import Wire.API.Event.Conversation
import Wire.API.Message
import Wire.API.Notification (QueuedNotification, queuedNotification, QueuedNotificationList, queuedNotificationList)
import Wire.API.Properties
-- import Wire.API.Provider
import Wire.API.Provider.Bot
import Wire.API.Provider.External
import Wire.API.Provider.Service
-- import Wire.API.Provider.Service.Tag
import Wire.API.Push.Token hiding (Transport)
import qualified Wire.API.Push.Token as Push.Token
import Wire.API.Team
import Wire.API.Team.Role
-- import Wire.API.Team.SearchVisibility
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Auth
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.User.Handle
import Wire.API.User.Identity
import Wire.API.User.Password
import Wire.API.User.Profile
import Wire.API.User.RichInfo
import Wire.API.User.Search
import Wire.API.Wrapped
testObject_UserUpdate_user_1 :: UserUpdate
testObject_UserUpdate_user_1 = UserUpdate {uupName = Nothing, uupPict = Just (Pict {fromPict = []}), uupAssets = Nothing, uupAccentId = Just (ColourId {fromColourId = -3})}
testObject_UserUpdate_user_2 :: UserUpdate
testObject_UserUpdate_user_2 = UserUpdate {uupName = Just (Name {fromName = "vf\v\\\999648l\73001\992762Q\USm\FS\DC2\STX\GS0\1055632g~!^QU0q\22057\EMW\25371\1042181)\SYN\ETX1\SYNE[JtgD\NUL\DC1c\173289BhT\158007_bSY\1034579xwMY\1061903\DLE/\74203f0`W\\]\1024300:*\180439R\USaV\\DQ1M\FS\157294\&8V:+\94370\ENQ\1072304o\"\139983S\DC3c\ETB\1017827\ao\FS\NULa\r\nbCd\RSk\34915\"Y\SOHN\SYNOn\147610Q\1003284\v1\1006153\100584."}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "~M" (Just AssetComplete)),(ImageAsset "\\\35317\r" (Just AssetPreview))], uupAccentId = Just (ColourId {fromColourId = -5})}
testObject_UserUpdate_user_3 :: UserUpdate
testObject_UserUpdate_user_3 = UserUpdate {uupName = Nothing, uupPict = Nothing, uupAssets = Nothing, uupAccentId = Just (ColourId {fromColourId = 0})}
testObject_UserUpdate_user_4 :: UserUpdate
testObject_UserUpdate_user_4 = UserUpdate {uupName = Just (Name {fromName = "\SOX\GSo_\78010T\SUB`OR1\1040567#\1061241vv~\SOH\ETX\29903\ESC\SOH\162630|\1016704\NUL\41695bl"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "\71233vF" (Just AssetComplete)),(ImageAsset "\ACKB\ETX" (Just AssetPreview))], uupAccentId = Just (ColourId {fromColourId = -6})}
testObject_UserUpdate_user_5 :: UserUpdate
testObject_UserUpdate_user_5 = UserUpdate {uupName = Just (Name {fromName = "\1002751da3\171148\7317"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [], uupAccentId = Just (ColourId {fromColourId = 6})}
testObject_UserUpdate_user_6 :: UserUpdate
testObject_UserUpdate_user_6 = UserUpdate {uupName = Just (Name {fromName = "'>+[\20322\r\63797\b\1075404$\175645R\DC4>h"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Nothing, uupAccentId = Just (ColourId {fromColourId = 5})}
testObject_UserUpdate_user_7 :: UserUpdate
testObject_UserUpdate_user_7 = UserUpdate {uupName = Just (Name {fromName = "J\1087776\1101728%$o)1\145803R\GS\\\13532\DC1E!\GSE\1039986e\DC2`\1049584R\DC3\98974XK/\ESCk\1103746\DLE\b\1032764\1101091C\1074580jx(\\~\STX\ENQ<\SIV\t"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "Q\SUB" (Just AssetPreview)),(ImageAsset "g" (Just AssetComplete)),(ImageAsset "" (Nothing))], uupAccentId = Just (ColourId {fromColourId = 6})}
testObject_UserUpdate_user_8 :: UserUpdate
testObject_UserUpdate_user_8 = UserUpdate {uupName = Just (Name {fromName = "\EM\147704i\r/!\STXO\SYN\1091776\176804\1050130v\1062987\FS5cB\136945i\EM\STX\DC2"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Nothing, uupAccentId = Just (ColourId {fromColourId = 7})}
testObject_UserUpdate_user_9 :: UserUpdate
testObject_UserUpdate_user_9 = UserUpdate {uupName = Just (Name {fromName = "\1025789Ddb\1017365\78762\95561\DC32t!s\44659\1091437g*Jbop*m=\DC3\1079817j\NULgR\SYNj\EM\1007599\35332\131787>\1076730pKJY\"XyG\44365"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Nothing, uupAccentId = Just (ColourId {fromColourId = -6})}
testObject_UserUpdate_user_10 :: UserUpdate
testObject_UserUpdate_user_10 = UserUpdate {uupName = Just (Name {fromName = "\EM\15633h\RSH\DC39J\136451D<\66776d7#\32868bv=3o\DEL\b\127850<{9&\f;w`"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [], uupAccentId = Nothing}
testObject_UserUpdate_user_11 :: UserUpdate
testObject_UserUpdate_user_11 = UserUpdate {uupName = Just (Name {fromName = "\EOTU'\1026290\1033562:9\RS\1045882R \28154\CAN\CAN\168574\120723\&3\1060055\987613 |\DC3-AU\RS\1112793ew\39898\CANxs\STX\f\SO*_\1070781$w\DC1\181912\1039418F\DLE\140756\73464\167468zGifyU\SO\42252|>\140144\139875,\46777t\1109164#\1011222dj\989519-\SUB\991148\EOT:"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "=[\\" (Just AssetComplete)),(ImageAsset "\35100\187476H" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Nothing))], uupAccentId = Nothing}
testObject_UserUpdate_user_12 :: UserUpdate
testObject_UserUpdate_user_12 = UserUpdate {uupName = Nothing, uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "9" (Nothing)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Nothing))], uupAccentId = Just (ColourId {fromColourId = 6})}
testObject_UserUpdate_user_13 :: UserUpdate
testObject_UserUpdate_user_13 = UserUpdate {uupName = Just (Name {fromName = "Vz\SOH\1061891\5168\991320[\ESCH\GS\17097H \US+6g\SI\GSH)\US\152985\EMN\999219\62108C\RS\1068914'x\1030443&vq-tKo\vCr\NAK\SO\r\DLEN\1083729\1019036r\DC3g~/\983633?\38629\DC1D*\8145\RSw\b#F]p\99467f1\39336\\dB\US\EM\27071|\22266)\185273\1103393;&~\RSe\NUL)[n]\RS\1010998yj\53147\1090237H\133280qtp\15744\GS\1078558"}), uupPict = Nothing, uupAssets = Just [], uupAccentId = Nothing}
testObject_UserUpdate_user_14 :: UserUpdate
testObject_UserUpdate_user_14 = UserUpdate {uupName = Just (Name {fromName = "Z\995617j\SOH\NAK61\ESCSN\EM\EOT\US\DC1Z\GS"}), uupPict = Nothing, uupAssets = Just [(ImageAsset "|\1052077" (Nothing)),(ImageAsset "\tY" (Just AssetComplete)),(ImageAsset "o\n\"" (Just AssetPreview)),(ImageAsset "JD" (Just AssetPreview)),(ImageAsset "\DLE" (Just AssetComplete))], uupAccentId = Just (ColourId {fromColourId = 3})}
testObject_UserUpdate_user_15 :: UserUpdate
testObject_UserUpdate_user_15 = UserUpdate {uupName = Just (Name {fromName = "y\178320\1092553\992319\SOH){\1051043\165811\ETB\149861\&14\CAN:t\GS\\7\1069976\SI1\186034!D\1063361B\157479-WbO!\1084039\f(\174855\&1\98592]gq\54781U\42396J*I\ESCf\119314\170071\1070960yD\185509\NUL\ESC\1025264+t\1070783\SO\123190\1047246\140531\132959>ky\DC2O\SI\168322kM\1072115n\1018933>\986767t\987135"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "Kz" (Just AssetComplete)),(ImageAsset "~" (Nothing)),(ImageAsset "\78667'\1002993" (Just AssetComplete)),(ImageAsset "\142305" (Just AssetComplete)),(ImageAsset "\988227^%" (Just AssetPreview)),(ImageAsset "\DC1\CANy" (Nothing)),(ImageAsset "" (Just AssetPreview))], uupAccentId = Nothing}
testObject_UserUpdate_user_16 :: UserUpdate
testObject_UserUpdate_user_16 = UserUpdate {uupName = Just (Name {fromName = "K;c3\1103279>6L\USN8]\164760\bgkPl\"{\DC4\1080975\ETX?\40278u\989623`\1017359\1046605\1010493\1084238DNn\1021075\tXc\984080K\1054153\127948>g"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "WA" (Nothing))], uupAccentId = Nothing}
testObject_UserUpdate_user_17 :: UserUpdate
testObject_UserUpdate_user_17 = UserUpdate {uupName = Just (Name {fromName = "\a\EM\177892ol4a\1045339\1056014V\1073622\&4V\917621/L--a^\36538r_g S.\SO=\STX\1081060l\13321\NAKJb$@KKDj\181362\&7\DC4H\1043527\ETB^&R{\STX|\\U\NAK\149862\1092592\ESC)W8\ACK(^u\ESC\DC1x~\DC4o{V"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview))], uupAccentId = Just (ColourId {fromColourId = -1})}
testObject_UserUpdate_user_18 :: UserUpdate
testObject_UserUpdate_user_18 = UserUpdate {uupName = Just (Name {fromName = "D0;\43142\t\1027330\179746ds;'\66691|t\128454\1073070(\41203B\DC4w_\NAK\72100\&7:bsT<\DC2*\EME\NUL\r\CANB\997744\1007800e-/{5\SUBn\58053-\5382\988209\137756g)\SYN\EMq%!V\EM\f\162822\vZj-KA\49364Xgh<\166253h\1057461\SUB\SO\38649'\138580\54094\54848hb\DC1rx\tfe\EOTU\1067086\SYNl\173947\189879o\21703U\31017\34575)-[\ENQ\ESC\f{2\98013\NUL^"}), uupPict = Nothing, uupAssets = Just [(ImageAsset "\1067989\FS" (Just AssetComplete)),(ImageAsset "_\149859%" (Nothing))], uupAccentId = Just (ColourId {fromColourId = -1})}
testObject_UserUpdate_user_19 :: UserUpdate
testObject_UserUpdate_user_19 = UserUpdate {uupName = Just (Name {fromName = "%\148333\28541[\DLEG\1021998\63321F{\"\EOTV]\ETX$\30614D2]\ETXUn6x\EOT_\113669B\1108372\a\29197\162604\&7\1054541sZ^;-P\v\1072390\53202\1056858\1076512\v2>jP\US\54753\STX\30279O4V\171387\189185/\1076003\EMl\\\ESC>/g\174968ao\1077962^#\DC1+\41990W:\1019731\NAK\DEL\127118"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "\NAK" (Just AssetComplete)),(ImageAsset "#" (Just AssetPreview)),(ImageAsset "\SOHi" (Just AssetPreview))], uupAccentId = Just (ColourId {fromColourId = -6})}
testObject_UserUpdate_user_20 :: UserUpdate
testObject_UserUpdate_user_20 = UserUpdate {uupName = Just (Name {fromName = "2\178485\rQl\EOT(\151472\&1\DC2\a\1017216\1027171\FS/\1042645c'N\1096523\&9APT$0+\55032\DLE\53764\DLE"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "h9" (Just AssetComplete)),(ImageAsset "" (Nothing)),(ImageAsset "\SOHq" (Nothing)),(ImageAsset "J}\145824" (Nothing)),(ImageAsset "\DEL\163534" (Just AssetPreview)),(ImageAsset "%\GS" (Nothing)),(ImageAsset "\SYN\19112" (Nothing))], uupAccentId = Just (ColourId {fromColourId = 3})}
