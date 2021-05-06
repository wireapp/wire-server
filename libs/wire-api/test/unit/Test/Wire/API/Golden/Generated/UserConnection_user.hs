{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UserConnection_user where

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
testObject_UserConnection_user_1 :: UserConnection
testObject_UserConnection_user_1 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000003"))), ucTo = (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000300000003"))), ucStatus = Accepted, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-08T10:46:18.739Z")), ucMessage = Just (Message {messageText = "\DC3\SYN\tx\GS]\DLEm!/\CANv-\DC2\186536+bS4yZ\986662K?*\n\1055083\98280\ENQ1\162813k9B\CAN\1030662\NAKL~T\141084\1036650&d\20380h!\185414P\189921\SIt\1091372\t\1050532\DC4=J{p-\984404\ETBve\EOT\SOH)!\1075026tQ\DC4t\DEL\t]\188648G\1019405;\DELK(\EM\1010585\DC4U`d~,\DC1\155953^\CAN\"M\STX\988411\DC1\36092\67208)\15382*\1031053D\73825"}), ucConvId = Nothing}
testObject_UserConnection_user_2 :: UserConnection
testObject_UserConnection_user_2 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002"))), ucTo = (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000300000001"))), ucStatus = Ignored, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-12T11:24:50.833Z")), ucMessage = Just (Message {messageText = "\1022770\180504x\CAN\1065430pL\1017334E>\1011670\&5"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000000000004")))}
testObject_UserConnection_user_3 :: UserConnection
testObject_UserConnection_user_3 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000100000004"))), ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000400000003"))), ucStatus = Ignored, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-14T09:55:23.400Z")), ucMessage = Just (Message {messageText = "6&:xcL\36572\NAKW6\tYi\1104720r^j9\31933\1089615\46951\21475\40509\f\1051537[\30490\SIlxS\v\1029621\&4o\97062y\NUL\178424;\5707\1067951\1036451T\1108396\ENQo\984806\&7\ETB\153090\38712\NAKg`$\178104\SUB\1060447z\n\t\1020220q\1028159!#G\1040704\RS\DC4@M\1091477\DC3\SI"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000100000003")))}
testObject_UserConnection_user_4 :: UserConnection
testObject_UserConnection_user_4 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000100000002"))), ucTo = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000100000001"))), ucStatus = Sent, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-13T10:55:56.457Z")), ucMessage = Just (Message {messageText = "\STX1g8g\FS|^lH\CAN\1067874g\fd\\w\v\RSfm5\987918\39286j\8369H|\54721\136209Qh\1022288Y\983301\1024148\1091722\1034356 ,\58607S\1089220'\SUB.m\RS\NUL\1025591#uG\FSu\\a1eLB\176275\1042748\NUL}\52912YXj6\"\1052306!V%^@\73665.)\15303\n\ENQdw(GN.CI\73825j/N\1005309\SI\ACK\44970[\SO}2!\1012322>x\1038002\99968\NAK\a\152143_\151572hm\1044172\1112282p\SOs\US\1030660\f\1006494}B\149502\EOT<\US\156034\991800l\1043050\1031974{g\1109958\&5\EOT\US?\1101498\&4@;8\SUB\135769Y \vGa\1096892\DC4\153959\FSs\1102626s\EM.u\1044042;4I\1054927x\FS\52698z\1005093q\177683L7F,\DC3\US\1106947t\1089882\21758\DLE\ESC\1087152\"\46863\STX1\ENQ=bx\SOH\1100213z7T&\46953\DC2o \SYNLT\128857Tk\DEL\SI\145107H\EOT`\60308@W_\1003224\DLE_\1087102\1078438V6[`\ENQ\3276\133464pAPA\145119b?zZ;"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000400000003")))}
testObject_UserConnection_user_5 :: UserConnection
testObject_UserConnection_user_5 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000300000003"))), ucTo = (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000100000000"))), ucStatus = Ignored, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-11T16:35:03.043Z")), ucMessage = Nothing, ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000004")))}
