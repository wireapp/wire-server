{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UserIdentity_user where

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
testObject_UserIdentity_1 :: UserIdentity
testObject_UserIdentity_1 = EmailIdentity (Email {emailLocal = "\147568qX<", emailDomain = "\SUBibW\1025158z[\31973\"(\SUB\NAKp\FS\155500\1046807!\1074476D\147863\1081794*Z+"})
testObject_UserIdentity_2 :: UserIdentity
testObject_UserIdentity_2 = EmailIdentity (Email {emailLocal = "t\1055294^!/\1083797\1048219\1043167\&80\1070083\168407xQ", emailDomain = "\128282\SI\1076635"})
testObject_UserIdentity_3 :: UserIdentity
testObject_UserIdentity_3 = PhoneIdentity (Phone {fromPhone = "+54420964279"})
testObject_UserIdentity_4 :: UserIdentity
testObject_UserIdentity_4 = EmailIdentity (Email {emailLocal = "z\US\26774\1034320\&1\1078181\SUB\ENQY\29383mwIqu\1103403;\1034920GB\ENQ5w+#", emailDomain = "\"2\EM/;j\1103483\96414l\1099964\63252(%^*\144869\NAK8\1032466"})
testObject_UserIdentity_5 :: UserIdentity
testObject_UserIdentity_5 = SSOIdentity (UserScimExternalId "]g&") (Just (Email {emailLocal = "&,x\987449\&0\SO", emailDomain = "~"})) (Just (Phone {fromPhone = "+30429628305"}))
