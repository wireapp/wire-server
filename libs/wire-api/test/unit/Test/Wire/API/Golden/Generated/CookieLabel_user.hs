{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.CookieLabel_user where

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
testObject_CookieLabel_user_1 :: CookieLabel
testObject_CookieLabel_user_1 = CookieLabel {cookieLabelText = "\ETBbi?XS\tT\38783\&73\27593\FS\142199\ETBP"}
testObject_CookieLabel_user_2 :: CookieLabel
testObject_CookieLabel_user_2 = CookieLabel {cookieLabelText = "\1106965LLp\DEL7B\1035088\1028441+8'\10182"}
testObject_CookieLabel_user_3 :: CookieLabel
testObject_CookieLabel_user_3 = CookieLabel {cookieLabelText = "\ETXV\176789\8951\152857\153701\&6?\SIN\63524wr\ENQP\180608."}
testObject_CookieLabel_user_4 :: CookieLabel
testObject_CookieLabel_user_4 = CookieLabel {cookieLabelText = "o/Y\999004OGN0\USU"}
testObject_CookieLabel_user_5 :: CookieLabel
testObject_CookieLabel_user_5 = CookieLabel {cookieLabelText = "3;\58400e<\SI\SI\1042691\136264N\DLE\189215/\53712\149338_)"}
testObject_CookieLabel_user_6 :: CookieLabel
testObject_CookieLabel_user_6 = CookieLabel {cookieLabelText = ",\EM\DEL\41672n\1050364/wc\NAKyo)6\22147U\100748"}
testObject_CookieLabel_user_7 :: CookieLabel
testObject_CookieLabel_user_7 = CookieLabel {cookieLabelText = "\FS\EM\1036492nVDF\DC1\1016522[p\NUL8I5\NAK^\NUL\DLE\46366J\US\f[@ "}
testObject_CookieLabel_user_8 :: CookieLabel
testObject_CookieLabel_user_8 = CookieLabel {cookieLabelText = "E5\SOH1\DC3\50199qd\111259_r\1079492f\38195\\NL2\58128\175583\SYN("}
testObject_CookieLabel_user_9 :: CookieLabel
testObject_CookieLabel_user_9 = CookieLabel {cookieLabelText = "\v\DC3\DC1"}
testObject_CookieLabel_user_10 :: CookieLabel
testObject_CookieLabel_user_10 = CookieLabel {cookieLabelText = "\18813\SI\CAN|\74148\&2\1044970\1104434MZB\37571yN\138319l\STX\154135\&0'"}
testObject_CookieLabel_user_11 :: CookieLabel
testObject_CookieLabel_user_11 = CookieLabel {cookieLabelText = "\1384QY"}
testObject_CookieLabel_user_12 :: CookieLabel
testObject_CookieLabel_user_12 = CookieLabel {cookieLabelText = "D\1025302\tw\995642*H\tK"}
testObject_CookieLabel_user_13 :: CookieLabel
testObject_CookieLabel_user_13 = CookieLabel {cookieLabelText = "\NULZ\382,\1049579\NUL&"}
testObject_CookieLabel_user_14 :: CookieLabel
testObject_CookieLabel_user_14 = CookieLabel {cookieLabelText = "\1008131NB\62607[iY\51583n\74818\&3\1010769\6738\tr\SYN\1013042\167621m\a\25558\EOT\ENQk\1038960t\ETBU"}
testObject_CookieLabel_user_15 :: CookieLabel
testObject_CookieLabel_user_15 = CookieLabel {cookieLabelText = "yK:+]|=\ENQk?\RS`:\1034480$\176826\64790x\1062876n\1043542"}
testObject_CookieLabel_user_16 :: CookieLabel
testObject_CookieLabel_user_16 = CookieLabel {cookieLabelText = "\70149"}
testObject_CookieLabel_user_17 :: CookieLabel
testObject_CookieLabel_user_17 = CookieLabel {cookieLabelText = "D\"A\176417+~%#f\DC1?\47748#"}
testObject_CookieLabel_user_18 :: CookieLabel
testObject_CookieLabel_user_18 = CookieLabel {cookieLabelText = "=*\DEL\EM\ESCs\131361\187938\158775,"}
testObject_CookieLabel_user_19 :: CookieLabel
testObject_CookieLabel_user_19 = CookieLabel {cookieLabelText = "G\1074975K7>\ETX\""}
testObject_CookieLabel_user_20 :: CookieLabel
testObject_CookieLabel_user_20 = CookieLabel {cookieLabelText = "E\DLE5[\f\EM\98046$"}
