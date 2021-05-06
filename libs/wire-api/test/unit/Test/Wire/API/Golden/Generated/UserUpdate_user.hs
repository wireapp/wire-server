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
testObject_UserUpdate_user_1 = UserUpdate {uupName = Just (Name {fromName = "*\177399\b\f\60112\148430G:\SYN\v\166382g\1598\1024312\1078256\40928\1073120Z\178085y*d\29613d\DC2\ACK0\NAK\b\SI\23591%\1112931b\1023583\ACKR\f,\EOTP\fT6?Q\174206\1014500_\1065781Z\15702\178890\SI5\1021942\n\"\1058561\"`u"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Nothing, uupAccentId = Just (ColourId {fromColourId = -6})}
testObject_UserUpdate_user_2 :: UserUpdate
testObject_UserUpdate_user_2 = UserUpdate {uupName = Just (Name {fromName = "\v\1086459l\1065956\1092536\EOT]H\989245\n/\v\bJ\4838Nhv\94053\&2\1042624\1010991/u\SOH/\bO\a\SOH\a\153924\&0\DEL\170873#\FS|hRp\v\20645W\1045138M\btfc\138461i\1113366&I\60840d\18589\1023130O=\1008280\&8"}), uupPict = Nothing, uupAssets = Just [(ImageAsset "6" (Nothing)),(ImageAsset "\EM" (Just AssetComplete)),(ImageAsset "" (Nothing))], uupAccentId = Nothing}
testObject_UserUpdate_user_3 :: UserUpdate
testObject_UserUpdate_user_3 = UserUpdate {uupName = Just (Name {fromName = "Z\1044507b\DEL\1018358aHpb\1056310K\nN\1105146\1005413+F\US\72126\22619\150740J\129559\1045020\34902YY\RS1xVR\993738+O7c\DLE\1030566\\\1108935\SO\DC3\RS+=\DEL"}), uupPict = Nothing, uupAssets = Just [], uupAccentId = Just (ColourId {fromColourId = -5})}
testObject_UserUpdate_user_4 :: UserUpdate
testObject_UserUpdate_user_4 = UserUpdate {uupName = Nothing, uupPict = Just (Pict {fromPict = []}), uupAssets = Just [], uupAccentId = Just (ColourId {fromColourId = -4})}
testObject_UserUpdate_user_5 :: UserUpdate
testObject_UserUpdate_user_5 = UserUpdate {uupName = Just (Name {fromName = "\ACK(/\46728i\FS \1078865/\164928)M\988815-\DC2\CAN\74322\v\b}\1077985@\166572\167807\ESCX:i\a\153642#\1021905!|\9383\STX\1044968Me\17497v\DEL~Di\1075257y\GS\DC2\1063865\ETX~\DC3`K\1099163B\1097998hH\SYN\DLE\1072258\1087312\39137\&6Jt\1013714m\1105522\1100740q\1107534\7550\181999{PlSu[uN\1101148{'kE\185760a\34685\1056886(H\1105443I\53878\1040116\&7\NAK\1030099\NUL*w\1042329$\"\1072612\1097445k\1053717\1060189^=K%"}), uupPict = Nothing, uupAssets = Just [(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "#" (Nothing))], uupAccentId = Just (ColourId {fromColourId = -8})}
