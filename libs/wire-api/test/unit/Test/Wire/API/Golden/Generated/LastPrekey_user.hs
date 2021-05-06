{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.LastPrekey_user where

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
testObject_LastPrekey_user_1 :: LastPrekey
testObject_LastPrekey_user_1 = (lastPrekey ("Z\65258A\ESCX?S}M\NAK\26611\"#\ENQ\STX\162867X\140411\EOT\ETB\127522\EMA\1014897\ACK\13343R+"))
testObject_LastPrekey_user_2 :: LastPrekey
testObject_LastPrekey_user_2 = (lastPrekey ("\SUB\1029777"))
testObject_LastPrekey_user_3 :: LastPrekey
testObject_LastPrekey_user_3 = (lastPrekey ("\159380\1057987ga"))
testObject_LastPrekey_user_4 :: LastPrekey
testObject_LastPrekey_user_4 = (lastPrekey (""))
testObject_LastPrekey_user_5 :: LastPrekey
testObject_LastPrekey_user_5 = (lastPrekey ("\989954\78627c"))
testObject_LastPrekey_user_6 :: LastPrekey
testObject_LastPrekey_user_6 = (lastPrekey ("`J\169381?\CAN\\\t\25345U}\1724"))
testObject_LastPrekey_user_7 :: LastPrekey
testObject_LastPrekey_user_7 = (lastPrekey ("3W=]\37433\22553\rI\f\147965\DC3/\1086473#\52358\1017535"))
testObject_LastPrekey_user_8 :: LastPrekey
testObject_LastPrekey_user_8 = (lastPrekey ("m\1077432p\984222IK\ETXu[['\72869\989799w\1019504\GS\166033\t\1099282\DC4I"))
testObject_LastPrekey_user_9 :: LastPrekey
testObject_LastPrekey_user_9 = (lastPrekey ("~J\SO)\b\137699\9123"))
testObject_LastPrekey_user_10 :: LastPrekey
testObject_LastPrekey_user_10 = (lastPrekey ("MpW\1042046.!u&L\1023955(/"))
testObject_LastPrekey_user_11 :: LastPrekey
testObject_LastPrekey_user_11 = (lastPrekey ("\183998\\\CAN\1081774\RSs\1080924\f\33877Z\""))
testObject_LastPrekey_user_12 :: LastPrekey
testObject_LastPrekey_user_12 = (lastPrekey (" \r\15783Qd\1046979\b&V\1016854"))
testObject_LastPrekey_user_13 :: LastPrekey
testObject_LastPrekey_user_13 = (lastPrekey ("kL"))
testObject_LastPrekey_user_14 :: LastPrekey
testObject_LastPrekey_user_14 = (lastPrekey ("iu^\30505\141734^\SYNS8\185920\ENQ\178748M5M"))
testObject_LastPrekey_user_15 :: LastPrekey
testObject_LastPrekey_user_15 = (lastPrekey ("G\150105p)8]\31506\EML'#%\1404#W.\25063\NUL\DC3x\SOE\n"))
testObject_LastPrekey_user_16 :: LastPrekey
testObject_LastPrekey_user_16 = (lastPrekey ("A`nd\1003155~\SYNRv`9N\176748xg\21667"))
testObject_LastPrekey_user_17 :: LastPrekey
testObject_LastPrekey_user_17 = (lastPrekey ("D\147260>&\b?\n\169425V\120404M{GP\CAN{\39584,j[m\93811\DC2\138213iCk"))
testObject_LastPrekey_user_18 :: LastPrekey
testObject_LastPrekey_user_18 = (lastPrekey ("\44702dS\f\US\141588G\tD!#\58724J\ENQ[L'5CZ\140834"))
testObject_LastPrekey_user_19 :: LastPrekey
testObject_LastPrekey_user_19 = (lastPrekey ("e\1035072+Z!\188031V+4\CANi\ETBa\1035018*\CAN\1076866"))
testObject_LastPrekey_user_20 :: LastPrekey
testObject_LastPrekey_user_20 = (lastPrekey ("7\20097F:\170102\1033675\vm"))
