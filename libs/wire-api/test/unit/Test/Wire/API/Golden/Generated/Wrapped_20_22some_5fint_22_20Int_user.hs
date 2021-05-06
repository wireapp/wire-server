{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Wrapped_20_22some_5fint_22_20Int_user where

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
testObject_Wrapped_20_22some_5fint_22_20Int_user_1 :: Wrapped "some_int" Int
testObject_Wrapped_20_22some_5fint_22_20Int_user_1 = Wrapped {unwrap = -3}
testObject_Wrapped_20_22some_5fint_22_20Int_user_2 :: Wrapped "some_int" Int
testObject_Wrapped_20_22some_5fint_22_20Int_user_2 = Wrapped {unwrap = 5}
testObject_Wrapped_20_22some_5fint_22_20Int_user_3 :: Wrapped "some_int" Int
testObject_Wrapped_20_22some_5fint_22_20Int_user_3 = Wrapped {unwrap = -29}
testObject_Wrapped_20_22some_5fint_22_20Int_user_4 :: Wrapped "some_int" Int
testObject_Wrapped_20_22some_5fint_22_20Int_user_4 = Wrapped {unwrap = -30}
testObject_Wrapped_20_22some_5fint_22_20Int_user_5 :: Wrapped "some_int" Int
testObject_Wrapped_20_22some_5fint_22_20Int_user_5 = Wrapped {unwrap = -20}
testObject_Wrapped_20_22some_5fint_22_20Int_user_6 :: Wrapped "some_int" Int
testObject_Wrapped_20_22some_5fint_22_20Int_user_6 = Wrapped {unwrap = -8}
testObject_Wrapped_20_22some_5fint_22_20Int_user_7 :: Wrapped "some_int" Int
testObject_Wrapped_20_22some_5fint_22_20Int_user_7 = Wrapped {unwrap = 22}
testObject_Wrapped_20_22some_5fint_22_20Int_user_8 :: Wrapped "some_int" Int
testObject_Wrapped_20_22some_5fint_22_20Int_user_8 = Wrapped {unwrap = 26}
testObject_Wrapped_20_22some_5fint_22_20Int_user_9 :: Wrapped "some_int" Int
testObject_Wrapped_20_22some_5fint_22_20Int_user_9 = Wrapped {unwrap = -11}
testObject_Wrapped_20_22some_5fint_22_20Int_user_10 :: Wrapped "some_int" Int
testObject_Wrapped_20_22some_5fint_22_20Int_user_10 = Wrapped {unwrap = 19}
testObject_Wrapped_20_22some_5fint_22_20Int_user_11 :: Wrapped "some_int" Int
testObject_Wrapped_20_22some_5fint_22_20Int_user_11 = Wrapped {unwrap = -30}
testObject_Wrapped_20_22some_5fint_22_20Int_user_12 :: Wrapped "some_int" Int
testObject_Wrapped_20_22some_5fint_22_20Int_user_12 = Wrapped {unwrap = -4}
testObject_Wrapped_20_22some_5fint_22_20Int_user_13 :: Wrapped "some_int" Int
testObject_Wrapped_20_22some_5fint_22_20Int_user_13 = Wrapped {unwrap = 13}
testObject_Wrapped_20_22some_5fint_22_20Int_user_14 :: Wrapped "some_int" Int
testObject_Wrapped_20_22some_5fint_22_20Int_user_14 = Wrapped {unwrap = -1}
testObject_Wrapped_20_22some_5fint_22_20Int_user_15 :: Wrapped "some_int" Int
testObject_Wrapped_20_22some_5fint_22_20Int_user_15 = Wrapped {unwrap = 18}
testObject_Wrapped_20_22some_5fint_22_20Int_user_16 :: Wrapped "some_int" Int
testObject_Wrapped_20_22some_5fint_22_20Int_user_16 = Wrapped {unwrap = 13}
testObject_Wrapped_20_22some_5fint_22_20Int_user_17 :: Wrapped "some_int" Int
testObject_Wrapped_20_22some_5fint_22_20Int_user_17 = Wrapped {unwrap = 21}
testObject_Wrapped_20_22some_5fint_22_20Int_user_18 :: Wrapped "some_int" Int
testObject_Wrapped_20_22some_5fint_22_20Int_user_18 = Wrapped {unwrap = -12}
testObject_Wrapped_20_22some_5fint_22_20Int_user_19 :: Wrapped "some_int" Int
testObject_Wrapped_20_22some_5fint_22_20Int_user_19 = Wrapped {unwrap = -23}
testObject_Wrapped_20_22some_5fint_22_20Int_user_20 :: Wrapped "some_int" Int
testObject_Wrapped_20_22some_5fint_22_20Int_user_20 = Wrapped {unwrap = -15}
