{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Name_user where

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
testObject_Name_user_1 :: Name
testObject_Name_user_1 = Name {fromName = "=\1067208w\1054556\GS\141033^F\GS\FSS'\1044390\ACKa\1015962\146425\"#?u\1004121\1107208\nI\1080356g-Dgd\83504\1029829\&4p\164676~-\41879\999542E\129309\127505\RS"}
testObject_Name_user_2 :: Name
testObject_Name_user_2 = Name {fromName = "mE\17239M\ETX\184225a\a\172669\SYN\142093\1015043\&9\1020447\1001269Zb\175629\SO\b&\96146Yp;\997312\1082750C \17332p\5664;\1073634}\ENQ\GS]\1037715\1012036}\EOTzK`&,\1038063\DC4\RS\1095125\&8=6t\ETX\1111027\DLE\1075652\1057600\&1@d\35364\26093\1033749\133179\21776\a\NUL\1019835&\1042571\SOH'(+\SYN\1048851\185448\NAK4"}
testObject_Name_user_3 :: Name
testObject_Name_user_3 = Name {fromName = "Pa:\171675n\DELZ\t-d0\170652\1110069\EOTlw{0e\1006760\989999EX\fHdc\US\SO\SI\r\983984,\990311\180473\164262z]4XL\51577l\CAN\SUB_\ENQ\DC4G\1061740\1043791\&5\1042170IU\186963\168336\CAN2\ESC\998386`\DC3\b\986612i\1103626FFaH\STX\RSJl&\\\53953P"}
testObject_Name_user_4 :: Name
testObject_Name_user_4 = Name {fromName = "\US\ACKv)M0\63331\EMF'\11406%\SOH\5750\121417n\132299a\30051\f6\STX\1005381\39274e\NAKx@\NAK]n\GS\150730\ETB\1058567\DC2/UTPc\SUB{`\1237o\1060561s\1059006\100687Z1ZX\40748.\r\\;\1043072\&9%e\153644KM\132311'T\ETB\1081721\NULD7Fi%>\161455\&9'\1096829P\a)\DC2\STX\120331\USy^\1107023\SI)G^\190347rV\f\SUB\58215\v\1070130\1034935i\STXl\1089374Kz+2\1081257WA\\gx\9919\986243a"}
testObject_Name_user_5 :: Name
testObject_Name_user_5 = Name {fromName = "\SI\83477i\a\ft"}
