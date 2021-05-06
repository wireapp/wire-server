{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.RichField_user where

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
testObject_RichField_user_1 :: RichField
testObject_RichField_user_1 = RichField {richFieldType = "^*E\f\RS\1049010P\SOH\1044879\&8\127114\178864e\DLE\24200rR", richFieldValue = "\1051334\ETBR\DC3#Z\1021434PT\1100838Oy\a,S\996436\26462[\EOTY\ENQ^}"}
testObject_RichField_user_2 :: RichField
testObject_RichField_user_2 = RichField {richFieldType = "", richFieldValue = "\167810H8PW+&WV\EMkM\v\1090531X;"}
testObject_RichField_user_3 :: RichField
testObject_RichField_user_3 = RichField {richFieldType = "\1050051\FS\53800H\136156\DLE\DEL\b3\1032032*\ACK\ESC\983296Yo\145529wt\1110443\1049172", richFieldValue = "\SOHf\15074\DC1xQ\DC2\\hG"}
testObject_RichField_user_4 :: RichField
testObject_RichField_user_4 = RichField {richFieldType = "[OZ=\a\148290<*~\ENQv\23596?\n\NAK\190826!\991514\174086\1077087\1050680", richFieldValue = "q3\FS\999197\GS\GS\1070037*\50034/"}
testObject_RichField_user_5 :: RichField
testObject_RichField_user_5 = RichField {richFieldType = "FUeM\ESC\SIT \STXd\17669\EOTNb}TB\DC3a\SI\1059339$#Q\66712\SOHd\147909", richFieldValue = "\"\160301AK\r\1029533\SO[\USm\SO4\1046313\n\DC1cc~\a\SO\SUB)\FS\1030114"}
