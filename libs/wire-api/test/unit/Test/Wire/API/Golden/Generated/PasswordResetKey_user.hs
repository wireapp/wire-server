{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.PasswordResetKey_user where

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
testObject_PasswordResetKey_user_1 :: PasswordResetKey
testObject_PasswordResetKey_user_1 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("ZcMG08Rv")))}
testObject_PasswordResetKey_user_2 :: PasswordResetKey
testObject_PasswordResetKey_user_2 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("EmI-3q3oJ0euiQgNQBE=")))}
testObject_PasswordResetKey_user_3 :: PasswordResetKey
testObject_PasswordResetKey_user_3 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("CYcw_tAyFcHH6czNevYPenBVv7o=")))}
testObject_PasswordResetKey_user_4 :: PasswordResetKey
testObject_PasswordResetKey_user_4 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("iZX5oqm6KMo3OnQ=")))}
testObject_PasswordResetKey_user_5 :: PasswordResetKey
testObject_PasswordResetKey_user_5 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("OI2hg2g=")))}
testObject_PasswordResetKey_user_6 :: PasswordResetKey
testObject_PasswordResetKey_user_6 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("EAw=")))}
testObject_PasswordResetKey_user_7 :: PasswordResetKey
testObject_PasswordResetKey_user_7 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("xRyfdbs3Io17pLPtnMATYJk4TbZ_")))}
testObject_PasswordResetKey_user_8 :: PasswordResetKey
testObject_PasswordResetKey_user_8 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("RQhWahJ_gNfIE3Wt44ait9roWT3b")))}
testObject_PasswordResetKey_user_9 :: PasswordResetKey
testObject_PasswordResetKey_user_9 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("8chTH4k1Uhxm")))}
testObject_PasswordResetKey_user_10 :: PasswordResetKey
testObject_PasswordResetKey_user_10 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("3wvgvAk_1IC_K330rezr")))}
testObject_PasswordResetKey_user_11 :: PasswordResetKey
testObject_PasswordResetKey_user_11 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("9i8InFXHAt0Gm0Np")))}
testObject_PasswordResetKey_user_12 :: PasswordResetKey
testObject_PasswordResetKey_user_12 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("Frv24l4x6BtyDHYrpFNRkhhzY-fBLQ==")))}
testObject_PasswordResetKey_user_13 :: PasswordResetKey
testObject_PasswordResetKey_user_13 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("00xxVfv-UZmgdJljtpUM")))}
testObject_PasswordResetKey_user_14 :: PasswordResetKey
testObject_PasswordResetKey_user_14 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("DZ0nOJhNron84VG0R_iweQ==")))}
testObject_PasswordResetKey_user_15 :: PasswordResetKey
testObject_PasswordResetKey_user_15 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("pU8=")))}
testObject_PasswordResetKey_user_16 :: PasswordResetKey
testObject_PasswordResetKey_user_16 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("ap0Hla4=")))}
testObject_PasswordResetKey_user_17 :: PasswordResetKey
testObject_PasswordResetKey_user_17 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("GtIwogDlXKSSqwIzcOjKbQ-ohN6QMOiFjWJr4w==")))}
testObject_PasswordResetKey_user_18 :: PasswordResetKey
testObject_PasswordResetKey_user_18 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("bRI=")))}
testObject_PasswordResetKey_user_19 :: PasswordResetKey
testObject_PasswordResetKey_user_19 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("ClqW7Lk17yEsLuIr9NTRrrl-p9Mg4EE=")))}
testObject_PasswordResetKey_user_20 :: PasswordResetKey
testObject_PasswordResetKey_user_20 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("7C2FUrDnUXnKc0sYbenQYhTbYgqVLgs=")))}
