{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.InvitationCode_user where

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
testObject_InvitationCode_user_1 :: InvitationCode
testObject_InvitationCode_user_1 = InvitationCode {fromInvitationCode = (fromRight undefined (validate ("oCyG4KY=")))}
testObject_InvitationCode_user_2 :: InvitationCode
testObject_InvitationCode_user_2 = InvitationCode {fromInvitationCode = (fromRight undefined (validate ("nIboSlov4A==")))}
testObject_InvitationCode_user_3 :: InvitationCode
testObject_InvitationCode_user_3 = InvitationCode {fromInvitationCode = (fromRight undefined (validate ("RxpVGRkm8OPZMsL6gdNBkZE8gzm1QLKnupPa_tY=")))}
testObject_InvitationCode_user_4 :: InvitationCode
testObject_InvitationCode_user_4 = InvitationCode {fromInvitationCode = (fromRight undefined (validate ("xUuCuQ==")))}
testObject_InvitationCode_user_5 :: InvitationCode
testObject_InvitationCode_user_5 = InvitationCode {fromInvitationCode = (fromRight undefined (validate ("HZbrSY-a1wCbX1GctPINrg==")))}
testObject_InvitationCode_user_6 :: InvitationCode
testObject_InvitationCode_user_6 = InvitationCode {fromInvitationCode = (fromRight undefined (validate ("f3njNQzB5SyFfKTy")))}
testObject_InvitationCode_user_7 :: InvitationCode
testObject_InvitationCode_user_7 = InvitationCode {fromInvitationCode = (fromRight undefined (validate ("hn_zIRr1235PsivnP7KjvVfHxN9ncSJ__W4=")))}
testObject_InvitationCode_user_8 :: InvitationCode
testObject_InvitationCode_user_8 = InvitationCode {fromInvitationCode = (fromRight undefined (validate ("LbnugxJ2")))}
testObject_InvitationCode_user_9 :: InvitationCode
testObject_InvitationCode_user_9 = InvitationCode {fromInvitationCode = (fromRight undefined (validate ("lLt4XMY=")))}
testObject_InvitationCode_user_10 :: InvitationCode
testObject_InvitationCode_user_10 = InvitationCode {fromInvitationCode = (fromRight undefined (validate ("q9E7fw==")))}
testObject_InvitationCode_user_11 :: InvitationCode
testObject_InvitationCode_user_11 = InvitationCode {fromInvitationCode = (fromRight undefined (validate ("Jul0yRNiGyywOmmOeSU7pBU00Q==")))}
testObject_InvitationCode_user_12 :: InvitationCode
testObject_InvitationCode_user_12 = InvitationCode {fromInvitationCode = (fromRight undefined (validate ("UdUSZ-0uuTJdjDvu0phGnI_jk5o1L2v_")))}
testObject_InvitationCode_user_13 :: InvitationCode
testObject_InvitationCode_user_13 = InvitationCode {fromInvitationCode = (fromRight undefined (validate ("SKIRd37k")))}
testObject_InvitationCode_user_14 :: InvitationCode
testObject_InvitationCode_user_14 = InvitationCode {fromInvitationCode = (fromRight undefined (validate ("fIVqz0rqyQ==")))}
testObject_InvitationCode_user_15 :: InvitationCode
testObject_InvitationCode_user_15 = InvitationCode {fromInvitationCode = (fromRight undefined (validate ("Mqlde2M=")))}
testObject_InvitationCode_user_16 :: InvitationCode
testObject_InvitationCode_user_16 = InvitationCode {fromInvitationCode = (fromRight undefined (validate ("ml8kD2bVr-SEFudhqLB_YspPSKWQ6jCAwA==")))}
testObject_InvitationCode_user_17 :: InvitationCode
testObject_InvitationCode_user_17 = InvitationCode {fromInvitationCode = (fromRight undefined (validate ("z8btdy-GrY-KftR70kmMijFKNmODPQ==")))}
testObject_InvitationCode_user_18 :: InvitationCode
testObject_InvitationCode_user_18 = InvitationCode {fromInvitationCode = (fromRight undefined (validate ("n3qy4FZX0cIKjQeHkmmxy3s=")))}
testObject_InvitationCode_user_19 :: InvitationCode
testObject_InvitationCode_user_19 = InvitationCode {fromInvitationCode = (fromRight undefined (validate ("uC17QkL4ZKqRvsycqFOXqQ==")))}
testObject_InvitationCode_user_20 :: InvitationCode
testObject_InvitationCode_user_20 = InvitationCode {fromInvitationCode = (fromRight undefined (validate ("gIsZ")))}
