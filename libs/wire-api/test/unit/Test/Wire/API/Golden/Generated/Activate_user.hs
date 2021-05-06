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
testObject_Activate_user_1 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+70243926765154"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("cfJNhQ==")))}, activateDryrun = True}
testObject_Activate_user_2 :: Activate
testObject_Activate_user_2 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+3576169017298"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("wyG0j4sHCg==")))}, activateDryrun = False}
testObject_Activate_user_3 :: Activate
testObject_Activate_user_3 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+742175845470"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("Q3Bt2aOqE8U=")))}, activateDryrun = True}
testObject_Activate_user_4 :: Activate
testObject_Activate_user_4 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+852172526076687"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("3rOBrjSs")))}, activateDryrun = False}
testObject_Activate_user_5 :: Activate
testObject_Activate_user_5 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("Yvnm")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("_sb4ooM=")))}, activateDryrun = True}
testObject_Activate_user_6 :: Activate
testObject_Activate_user_6 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("DuhWUlGr")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("sO0=")))}, activateDryrun = False}
testObject_Activate_user_7 :: Activate
testObject_Activate_user_7 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+963772827568953"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("wqOPeg==")))}, activateDryrun = True}
testObject_Activate_user_8 :: Activate
testObject_Activate_user_8 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("ZQ==")))}, activateDryrun = False}
testObject_Activate_user_9 :: Activate
testObject_Activate_user_9 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+11984977169"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("WuV27OF5l6xY")))}, activateDryrun = False}
testObject_Activate_user_10 :: Activate
testObject_Activate_user_10 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("FGRGsXHcTc8=")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("hg==")))}, activateDryrun = True}
testObject_Activate_user_11 :: Activate
testObject_Activate_user_11 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("3PE=")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("")))}, activateDryrun = True}
testObject_Activate_user_12 :: Activate
testObject_Activate_user_12 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("7ATVGN4=")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("ULUP")))}, activateDryrun = True}
testObject_Activate_user_13 :: Activate
testObject_Activate_user_13 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("exNuomDWzA==")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("Ng==")))}, activateDryrun = False}
testObject_Activate_user_14 :: Activate
testObject_Activate_user_14 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("4g==")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("4Q==")))}, activateDryrun = True}
testObject_Activate_user_15 :: Activate
testObject_Activate_user_15 = Activate {activateTarget = ActivateEmail (Email {emailLocal = ".", emailDomain = "*-"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("SygNPjI=")))}, activateDryrun = False}
testObject_Activate_user_16 :: Activate
testObject_Activate_user_16 = Activate {activateTarget = ActivateEmail (Email {emailLocal = "j\DELIG", emailDomain = "\DC1J\1044425}"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("xx_8gToowX4=")))}, activateDryrun = True}
testObject_Activate_user_17 :: Activate
testObject_Activate_user_17 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+8285194492"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("uN7StUs=")))}, activateDryrun = False}
testObject_Activate_user_18 :: Activate
testObject_Activate_user_18 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+25239306989563"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("aory9UOrD0w=")))}, activateDryrun = False}
testObject_Activate_user_19 :: Activate
testObject_Activate_user_19 = Activate {activateTarget = ActivateEmail (Email {emailLocal = "'\t\50539;", emailDomain = "\1043387\1617\42792\nfp\SI"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("Nfeq")))}, activateDryrun = True}
testObject_Activate_user_20 :: Activate
testObject_Activate_user_20 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+52030937207731"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("jWqV")))}, activateDryrun = True}
