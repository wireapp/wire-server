{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Email_user where

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
testObject_Email_user_1 :: Email
testObject_Email_user_1 = Email {emailLocal = ".\a\1051265\14145\SI", emailDomain = "f\DC1\SI\59558\184627\SOJ\1019940\1042056\DEL\165573a\31273\167617{"}
testObject_Email_user_2 :: Email
testObject_Email_user_2 = Email {emailLocal = "\EM\175217E\ENQ\53837R\194752XKy\DC4B}\SYN.\DC1d6e\US\71108`r-\1101203`\1046634", emailDomain = "&+b|\SYNF\1103752]\ESCe\137641P\EOT\\\ETXe\1001719<*\ETX"}
testObject_Email_user_3 :: Email
testObject_Email_user_3 = Email {emailLocal = "\r\RS\DC3\ETBg3\175987\SUB\96657\\\US\\P\39161\ETBca*Y\ESC\US;\b\54794[O\FSm", emailDomain = "\a\DC2#!\1036258D\GS\ETB"}
testObject_Email_user_4 :: Email
testObject_Email_user_4 = Email {emailLocal = "H\SI1,c!\n\1096117\1073209\NUL\1038364E)\1072952[", emailDomain = "G\988254uV&u=\1083476:n\39144\1030440\ETB\SI\CAN\162024[\STX\fV6"}
testObject_Email_user_5 :: Email
testObject_Email_user_5 = Email {emailLocal = "\5450N\1034758q#\180143z", emailDomain = "\54137O\143798\1015185P\194561\NUL%\8611\1031626YkQ(\NAK"}
