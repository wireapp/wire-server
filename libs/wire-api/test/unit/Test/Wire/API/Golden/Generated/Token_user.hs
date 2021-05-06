{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Token_user where

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
testObject_Token_user_1 :: Token
testObject_Token_user_1 = Token {tokenText = "$+\DC2c=Y?,vy\19272U\164317\1107782\NAK&\145778b"}
testObject_Token_user_2 :: Token
testObject_Token_user_2 = Token {tokenText = "%\151298\35045\&7"}
testObject_Token_user_3 :: Token
testObject_Token_user_3 = Token {tokenText = "\25167\&4qI\1103378"}
testObject_Token_user_4 :: Token
testObject_Token_user_4 = Token {tokenText = "$]\998625"}
testObject_Token_user_5 :: Token
testObject_Token_user_5 = Token {tokenText = "H\1012386\&1\984626i\98548"}
testObject_Token_user_6 :: Token
testObject_Token_user_6 = Token {tokenText = "K0"}
testObject_Token_user_7 :: Token
testObject_Token_user_7 = Token {tokenText = "\RS1\30745d\165909\176691\&1\1058450\&4F\CAN\148707\&36UZ#\EOT\r\1099656\1013689+\181477(+\25556\ACK\127556\1042982t"}
testObject_Token_user_8 :: Token
testObject_Token_user_8 = Token {tokenText = "gH$\1020452\SOHQ{c"}
testObject_Token_user_9 :: Token
testObject_Token_user_9 = Token {tokenText = "l\ESC!j\DC3\35329\f\72974\DC2\171361}\EOT=po7]o\1039612\SO\174174\&6\RS2\\\175341\ENQ\v"}
testObject_Token_user_10 :: Token
testObject_Token_user_10 = Token {tokenText = "\1058842,|\thn\f\SOH}\1001908\aA"}
testObject_Token_user_11 :: Token
testObject_Token_user_11 = Token {tokenText = "\1090102"}
testObject_Token_user_12 :: Token
testObject_Token_user_12 = Token {tokenText = "\GSK\ESC\64738\1036769\27620\987881AAI\ETB%4\NUL"}
testObject_Token_user_13 :: Token
testObject_Token_user_13 = Token {tokenText = "\ETXk\SI\181275=n\1110889ll\1012219xY\1068486Z>\1063939"}
testObject_Token_user_14 :: Token
testObject_Token_user_14 = Token {tokenText = "0\DLE\33256o]\DC4\174362\a\1022373A9"}
testObject_Token_user_15 :: Token
testObject_Token_user_15 = Token {tokenText = "Tj\DC3\989973VX\8530 \1038822I\1046896\17569\92283\"f\DLE\1033815\1108486^\1112394]4\SIp\139167"}
testObject_Token_user_16 :: Token
testObject_Token_user_16 = Token {tokenText = "\NUL\174709jg\FS\1080907uS\DC3\DC3Y\tU\137631p/^\990286\995612g\1080163@\1041740UP\185344zHI\ETX"}
testObject_Token_user_17 :: Token
testObject_Token_user_17 = Token {tokenText = "X\170393z\34623\STX[\1036494\\\1103413@\1010739\60990\1108137/\ACK\52316i\EM\131659 \984497U\993259"}
testObject_Token_user_18 :: Token
testObject_Token_user_18 = Token {tokenText = "\SO\176500c&\178615\USf\170242\1050367"}
testObject_Token_user_19 :: Token
testObject_Token_user_19 = Token {tokenText = "\DEL\1096473"}
testObject_Token_user_20 :: Token
testObject_Token_user_20 = Token {tokenText = "fRx \1087559\63893\5627}N\94612Eb"}
