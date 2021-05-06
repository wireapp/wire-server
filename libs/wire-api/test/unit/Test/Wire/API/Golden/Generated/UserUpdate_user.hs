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
testObject_UserUpdate_user_1 = UserUpdate {uupName = Just (Name {fromName = "a\SUBN\STXS0l#\994261f\n'Y\NAK=\RSsv~\53687\&8\SUB\DLE|_\DC4\DELn4\10215+"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Nothing, uupAccentId = Nothing}
testObject_UserUpdate_user_2 :: UserUpdate
testObject_UserUpdate_user_2 = UserUpdate {uupName = Just (Name {fromName = "HH\nyWt-W\101064\995032j1u\25767c,\46301\ETX.\ETX\120274 _\n\165138w\SYN\n\45598\163026&\189998`+\RS\DLE\1096607\92993N\NUL\DC2\SOHv\NUL~9\185012\ACK\"\1032731.\1041155}J\22639{\NUL\7460~S4\DC1<1\GS\STX\ACKJmRZ\DEL[\1045368 \a\1109798p\SO~n\SOH~\b\1013241\GS\126978\DLE\1031687\1066675\167592\1073026"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete))], uupAccentId = Just (ColourId {fromColourId = -4})}
testObject_UserUpdate_user_3 :: UserUpdate
testObject_UserUpdate_user_3 = UserUpdate {uupName = Just (Name {fromName = "\ESCJ\1057258\1097311|\998821h\1003529\25754\FS\SYN\ETB\DC4$lY\1029222>^$I\r\1001649\DC2\n\119848t0\EM#^;\EMsc\1099432\a\34434qbj\SIVL\RS\bR\ENQ\1044893jDj\31474|$\CANQe^i\EOT\SUB\1063919\1000962+\DC3\DC3\1083117\&10\1040859Mv\70025\1049273U\SOdK\DC1\SOHt;52\NUL\161561q."}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "z>\190452" (Just AssetComplete)),(ImageAsset "\1022888a:" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete))], uupAccentId = Nothing}
testObject_UserUpdate_user_4 :: UserUpdate
testObject_UserUpdate_user_4 = UserUpdate {uupName = Just (Name {fromName = "\1064535\1017110\35995\1110423PCya\ENQ-Y\166269J\1011460\1085748\EM\ACKfN\1013276\SOH\151367B'\23333~6;\"v]|^\DC3\174257b\165132\1034795\FS=?;Moxi\ENQt&\SO\r!\151533\SI\67970\DC4\ACKBF\171589o\b\1101651\60292\RS:vQ: |DO\1068295>)Fg\DC1-e\988484UU$7_:i^o\37199\185344]\54995#C\1084696?P\95535\FS\1011398Gn\tm\NULP0wM\DC4@\1022205\"\SYNB\24004\NUL\STXwQTx\ESCq"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Nothing, uupAccentId = Just (ColourId {fromColourId = -2})}
testObject_UserUpdate_user_5 :: UserUpdate
testObject_UserUpdate_user_5 = UserUpdate {uupName = Nothing, uupPict = Just (Pict {fromPict = []}), uupAssets = Nothing, uupAccentId = Just (ColourId {fromColourId = 4})}
testObject_UserUpdate_user_6 :: UserUpdate
testObject_UserUpdate_user_6 = UserUpdate {uupName = Nothing, uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "\ENQ\SUB" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Nothing)),(ImageAsset "^M" (Nothing)),(ImageAsset "\179636" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "T" (Just AssetPreview))], uupAccentId = Just (ColourId {fromColourId = -3})}
testObject_UserUpdate_user_7 :: UserUpdate
testObject_UserUpdate_user_7 = UserUpdate {uupName = Nothing, uupPict = Just (Pict {fromPict = []}), uupAssets = Nothing, uupAccentId = Nothing}
testObject_UserUpdate_user_8 :: UserUpdate
testObject_UserUpdate_user_8 = UserUpdate {uupName = Nothing, uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "i" (Just AssetPreview)),(ImageAsset "\1030825" (Just AssetPreview)),(ImageAsset "\t" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview))], uupAccentId = Just (ColourId {fromColourId = 3})}
testObject_UserUpdate_user_9 :: UserUpdate
testObject_UserUpdate_user_9 = UserUpdate {uupName = Nothing, uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "\1086154t" (Just AssetPreview)),(ImageAsset "" (Nothing)),(ImageAsset "3i\68013" (Nothing)),(ImageAsset "WT\185498" (Just AssetPreview)),(ImageAsset "O\987298\1047040" (Just AssetComplete)),(ImageAsset "}\SOH\"" (Nothing)),(ImageAsset "" (Just AssetPreview))], uupAccentId = Nothing}
testObject_UserUpdate_user_10 :: UserUpdate
testObject_UserUpdate_user_10 = UserUpdate {uupName = Just (Name {fromName = "\ETX\1060674\995623O\RS\1085124(CF\ETB\1089763\&2\EM\1111911\119011\110724\NUL!(XJS/\SYNK\44480Tn#\SYNQ\1056549\&2\RSd:\1072125\ESC' :F\FSzS6*j\ETB^\DC3yRe8\47526\73744n\SO\153803\1070566\n4}YvuGP\1004222Q6\DEL\1030470\CANJ\bZ\1083335y\ETB \988511\917958ePSd\DELwL\RS\t^1]\DC4y3jV<^i/-\159691A\1049810}\63161\DC1~=Acf~_YBlhw"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "8IA" (Just AssetPreview)),(ImageAsset "" (Nothing)),(ImageAsset "\ETBJW" (Just AssetPreview)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Nothing)),(ImageAsset "6" (Just AssetComplete))], uupAccentId = Just (ColourId {fromColourId = 5})}
testObject_UserUpdate_user_11 :: UserUpdate
testObject_UserUpdate_user_11 = UserUpdate {uupName = Nothing, uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "7" (Just AssetPreview))], uupAccentId = Just (ColourId {fromColourId = -8})}
testObject_UserUpdate_user_12 :: UserUpdate
testObject_UserUpdate_user_12 = UserUpdate {uupName = Just (Name {fromName = "T@\SUB\SI\vC\ETX\1097152\1077757\155356\SOHR/\DC35\1010487O!\USV\DC3`\993428]\1098969\DLE\186384k7 0\DC1U\1009130<U\1014921v(\1078774\1020172u\SYN"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [], uupAccentId = Just (ColourId {fromColourId = 7})}
testObject_UserUpdate_user_13 :: UserUpdate
testObject_UserUpdate_user_13 = UserUpdate {uupName = Nothing, uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "d\1064817" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "HPX" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "#" (Nothing))], uupAccentId = Nothing}
testObject_UserUpdate_user_14 :: UserUpdate
testObject_UserUpdate_user_14 = UserUpdate {uupName = Just (Name {fromName = "v\ETX\ACK\ETXC\1074214\SUB\r\RSv'h;#BKD\1090765\140775\GS{&V2\1044444>\2539Z\DC1Z\1037506\61871 s\NAK\STX*+\n\DC3n\NUL@$\EM\24165K \1091504g\RSXv-\ENQ\\\63390\ENQ+\97829%~\ETX+\ACKy%j\11734\165309\DC2\DEL\v*Y%5\984024e\5787\ETB4\SO5+}\994510oR\DC4\1058812|\SOH\GS\EM\NAKS\bU[c{)`"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "\120753" (Nothing)),(ImageAsset "tO" (Just AssetPreview)),(ImageAsset ":^^" (Just AssetComplete)),(ImageAsset "H" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "S" (Nothing)),(ImageAsset "" (Nothing))], uupAccentId = Nothing}
testObject_UserUpdate_user_15 :: UserUpdate
testObject_UserUpdate_user_15 = UserUpdate {uupName = Just (Name {fromName = "\1084799\ENQaI\987283]\60403Gfo\1092660f\1103951\ENQ{\DELb\SI%\991693\1072549QG2\18680\&4w2&\SYN\1029196\ESCM%p\136579\vG=\DLE,\1034558-v\98980pC.\7465\64637N\69617\ak"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "g" (Nothing)),(ImageAsset "z\v\EOT" (Just AssetComplete)),(ImageAsset "\DC1\GSk" (Just AssetComplete))], uupAccentId = Just (ColourId {fromColourId = -3})}
testObject_UserUpdate_user_16 :: UserUpdate
testObject_UserUpdate_user_16 = UserUpdate {uupName = Nothing, uupPict = Nothing, uupAssets = Just [(ImageAsset "\1016126\ETB" (Just AssetComplete)),(ImageAsset "\160075\1053402" (Just AssetComplete)),(ImageAsset "\178794" (Just AssetPreview)),(ImageAsset "8\DC3" (Just AssetPreview)),(ImageAsset "]\a" (Just AssetComplete)),(ImageAsset ">" (Just AssetPreview)),(ImageAsset "\182632\&0\1104915" (Nothing))], uupAccentId = Just (ColourId {fromColourId = -2})}
testObject_UserUpdate_user_17 :: UserUpdate
testObject_UserUpdate_user_17 = UserUpdate {uupName = Just (Name {fromName = "e\DELW\DC3\46155\DC30\CANwdY\62704W\US\r:Q\1090406:#^\ACK\n\169096n*oUk"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "\1045764\1082993\1001513" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "Ph" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview))], uupAccentId = Just (ColourId {fromColourId = -4})}
testObject_UserUpdate_user_18 :: UserUpdate
testObject_UserUpdate_user_18 = UserUpdate {uupName = Nothing, uupPict = Nothing, uupAssets = Just [], uupAccentId = Nothing}
testObject_UserUpdate_user_19 :: UserUpdate
testObject_UserUpdate_user_19 = UserUpdate {uupName = Just (Name {fromName = "\STX/\b\137928@\1003291A\154300\SIJeq!@M\RS(\1090604S8K\DEL\986618< \DC1:~K\132501\FSw\1113675&A(4g\f\SOHA\aP\EMZ\1061381\ESCUj#cB#f\149916\ACKyCr@sNN\1053479?m>\140071V\1113894u\ETB\36823\43283N\2817,Bh?o6=\1090639\NAKZ`wO4dG\1012057-\51714'\ETBfN\74819\31243thx2\1016088:\1102696\ESC\29519;HS\138532VM/\CAN\aq\46681R2"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "\DLE\1085501" (Nothing)),(ImageAsset "\8640\147438z" (Just AssetPreview)),(ImageAsset "xP" (Just AssetPreview))], uupAccentId = Just (ColourId {fromColourId = -1})}
testObject_UserUpdate_user_20 :: UserUpdate
testObject_UserUpdate_user_20 = UserUpdate {uupName = Just (Name {fromName = "MoXL\142200G\SUB\STX\SO\164493\ACK*G+8U\145707q"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [], uupAccentId = Just (ColourId {fromColourId = -7})}
