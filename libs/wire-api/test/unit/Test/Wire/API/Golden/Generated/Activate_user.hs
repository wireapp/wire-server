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
testObject_Activate_user_1 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("BE8LdMk=")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("tajBcA-wMg==")))}, activateDryrun = False}
testObject_Activate_user_2 :: Activate
testObject_Activate_user_2 = Activate {activateTarget = ActivateEmail (Email {emailLocal = "&\1109906", emailDomain = "\ENQX\1049434\126582"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("wyVKabbJ47s=")))}, activateDryrun = True}
testObject_Activate_user_3 :: Activate
testObject_Activate_user_3 = Activate {activateTarget = ActivateEmail (Email {emailLocal = "<\SI\984625", emailDomain = "\ETBc"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("y2zCUKc=")))}, activateDryrun = False}
testObject_Activate_user_4 :: Activate
testObject_Activate_user_4 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("q-NM")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("ZB4=")))}, activateDryrun = True}
testObject_Activate_user_5 :: Activate
testObject_Activate_user_5 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+607379293"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("c70=")))}, activateDryrun = True}
testObject_Activate_user_6 :: Activate
testObject_Activate_user_6 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("k9jmzzk=")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("h9McjCU=")))}, activateDryrun = True}
testObject_Activate_user_7 :: Activate
testObject_Activate_user_7 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+511456803"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("6GTABd8=")))}, activateDryrun = True}
testObject_Activate_user_8 :: Activate
testObject_Activate_user_8 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("Z_I=")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("E0ClWKcHt6E=")))}, activateDryrun = True}
testObject_Activate_user_9 :: Activate
testObject_Activate_user_9 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+15268543750942"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("oo_mwnhRFg==")))}, activateDryrun = False}
testObject_Activate_user_10 :: Activate
testObject_Activate_user_10 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("7cZ4")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("")))}, activateDryrun = False}
testObject_Activate_user_11 :: Activate
testObject_Activate_user_11 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("hlnHyYc=")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("")))}, activateDryrun = True}
testObject_Activate_user_12 :: Activate
testObject_Activate_user_12 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("bLlJ6_h6Cw==")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("xzrnuyBhBLk=")))}, activateDryrun = True}
testObject_Activate_user_13 :: Activate
testObject_Activate_user_13 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("L45Htbk=")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("iw==")))}, activateDryrun = True}
testObject_Activate_user_14 :: Activate
testObject_Activate_user_14 = Activate {activateTarget = ActivateEmail (Email {emailLocal = "", emailDomain = ""}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("vw==")))}, activateDryrun = False}
testObject_Activate_user_15 :: Activate
testObject_Activate_user_15 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("Ng==")))}, activateDryrun = False}
testObject_Activate_user_16 :: Activate
testObject_Activate_user_16 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("uw==")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("IYI=")))}, activateDryrun = True}
testObject_Activate_user_17 :: Activate
testObject_Activate_user_17 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+83308624530"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("fe9WkJhz")))}, activateDryrun = False}
testObject_Activate_user_18 :: Activate
testObject_Activate_user_18 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+0358004712"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("")))}, activateDryrun = False}
testObject_Activate_user_19 :: Activate
testObject_Activate_user_19 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("P5SWJtxfKjM=")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("WgJkJ2O4HQ==")))}, activateDryrun = True}
testObject_Activate_user_20 :: Activate
testObject_Activate_user_20 = Activate {activateTarget = ActivateEmail (Email {emailLocal = "", emailDomain = "\DC2\1100128"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("ym6_6Btoxiyi")))}, activateDryrun = False}
