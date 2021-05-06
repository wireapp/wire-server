{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Activate_user where

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
testObject_Activate_user_1 :: Activate
testObject_Activate_user_1 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+65200360213083"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("SpQEvYZUfKta")))}, activateDryrun = True}
testObject_Activate_user_2 :: Activate
testObject_Activate_user_2 = Activate {activateTarget = ActivateEmail (Email {emailLocal = "\1022771\15605", emailDomain = "\SYN"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("596fpKw=")))}, activateDryrun = False}
testObject_Activate_user_3 :: Activate
testObject_Activate_user_3 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("bC_zUC5eUA==")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("LhMRYw==")))}, activateDryrun = True}
testObject_Activate_user_4 :: Activate
testObject_Activate_user_4 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+6451197402"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("WOc-1eueQSE=")))}, activateDryrun = True}
testObject_Activate_user_5 :: Activate
testObject_Activate_user_5 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+5444127676595"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("eoQPeGQ=")))}, activateDryrun = True}
testObject_Activate_user_6 :: Activate
testObject_Activate_user_6 = Activate {activateTarget = ActivateEmail (Email {emailLocal = "T(E\1020196c\NAK\v", emailDomain = "E|"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("ifoeo8xE")))}, activateDryrun = True}
testObject_Activate_user_7 :: Activate
testObject_Activate_user_7 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+528215883"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("")))}, activateDryrun = True}
testObject_Activate_user_8 :: Activate
testObject_Activate_user_8 = Activate {activateTarget = ActivateEmail (Email {emailLocal = "\1025437", emailDomain = "\175026=&c<"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("")))}, activateDryrun = True}
testObject_Activate_user_9 :: Activate
testObject_Activate_user_9 = Activate {activateTarget = ActivateEmail (Email {emailLocal = "78\110666\SO\rc\1060093", emailDomain = "\44419>"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("rQ==")))}, activateDryrun = True}
testObject_Activate_user_10 :: Activate
testObject_Activate_user_10 = Activate {activateTarget = ActivateEmail (Email {emailLocal = "\1078754\SYN", emailDomain = "Z\t\t"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("Z9V8IQ==")))}, activateDryrun = True}
testObject_Activate_user_11 :: Activate
testObject_Activate_user_11 = Activate {activateTarget = ActivateEmail (Email {emailLocal = "\ETB'2\ACK\DC4\1111159", emailDomain = "\994685\1019588\US`2#\ESC"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("EkNrgmg-GA==")))}, activateDryrun = True}
testObject_Activate_user_12 :: Activate
testObject_Activate_user_12 = Activate {activateTarget = ActivateEmail (Email {emailLocal = "\996237\DC3T<h\v\14022", emailDomain = "Jw{\SO\DC2\SOH"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("C8Y04dQ195Ib")))}, activateDryrun = True}
testObject_Activate_user_13 :: Activate
testObject_Activate_user_13 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+509722827"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("erKm")))}, activateDryrun = False}
testObject_Activate_user_14 :: Activate
testObject_Activate_user_14 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("7cJeSJOlEQ8=")))}, activateDryrun = True}
testObject_Activate_user_15 :: Activate
testObject_Activate_user_15 = Activate {activateTarget = ActivateEmail (Email {emailLocal = "\183410z", emailDomain = "\173229+(\GSe_9"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("")))}, activateDryrun = True}
testObject_Activate_user_16 :: Activate
testObject_Activate_user_16 = Activate {activateTarget = ActivateEmail (Email {emailLocal = "", emailDomain = "+\1014661\58318|HG"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("G_TO96BawA==")))}, activateDryrun = False}
testObject_Activate_user_17 :: Activate
testObject_Activate_user_17 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("OZFDTA==")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("bH-6bnjzRA==")))}, activateDryrun = False}
testObject_Activate_user_18 :: Activate
testObject_Activate_user_18 = Activate {activateTarget = ActivateEmail (Email {emailLocal = "P\177140", emailDomain = ""}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("YksjgCkt7gCP")))}, activateDryrun = False}
testObject_Activate_user_19 :: Activate
testObject_Activate_user_19 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+38287104271"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("ww==")))}, activateDryrun = True}
testObject_Activate_user_20 :: Activate
testObject_Activate_user_20 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("2cGmAz0cVA==")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("DeW952c=")))}, activateDryrun = False}
