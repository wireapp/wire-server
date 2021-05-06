{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.NewPasswordReset_user where

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
testObject_NewPasswordReset_user_1 :: NewPasswordReset
testObject_NewPasswordReset_user_1 = NewPasswordReset (Left (Email {emailLocal = "~A\ENQ\26847HR\DC4\1005964\EOT\138202\&2`\aM`\US4", emailDomain = "\1083809JV*M1\997514\DC3M\DC2\190173\NULhnw\57476\NUL\ETX\ETX\EOT\2596\&7\188649"}))
testObject_NewPasswordReset_user_2 :: NewPasswordReset
testObject_NewPasswordReset_user_2 = NewPasswordReset (Left (Email {emailLocal = "t\1009213\191289o2sIA\1105418>\194965U\65721", emailDomain = "\53856!'\19672,+"}))
testObject_NewPasswordReset_user_3 :: NewPasswordReset
testObject_NewPasswordReset_user_3 = NewPasswordReset (Right (Phone {fromPhone = "+7223234305152"}))
testObject_NewPasswordReset_user_4 :: NewPasswordReset
testObject_NewPasswordReset_user_4 = NewPasswordReset (Right (Phone {fromPhone = "+623057707042512"}))
testObject_NewPasswordReset_user_5 :: NewPasswordReset
testObject_NewPasswordReset_user_5 = NewPasswordReset (Left (Email {emailLocal = "\77856\ffTV`cwJ.[n_L3\1045079\SO\31957\1068861|\1081451\STX", emailDomain = "\tY\rI\1036875i\EOT>e\ACK\1025259W\t\nU\1040351\ENQ\151930\CAN\EM"}))
testObject_NewPasswordReset_user_6 :: NewPasswordReset
testObject_NewPasswordReset_user_6 = NewPasswordReset (Right (Phone {fromPhone = "+429560623600"}))
testObject_NewPasswordReset_user_7 :: NewPasswordReset
testObject_NewPasswordReset_user_7 = NewPasswordReset (Right (Phone {fromPhone = "+79352417451"}))
testObject_NewPasswordReset_user_8 :: NewPasswordReset
testObject_NewPasswordReset_user_8 = NewPasswordReset (Right (Phone {fromPhone = "+084132509550680"}))
testObject_NewPasswordReset_user_9 :: NewPasswordReset
testObject_NewPasswordReset_user_9 = NewPasswordReset (Right (Phone {fromPhone = "+278236507027799"}))
testObject_NewPasswordReset_user_10 :: NewPasswordReset
testObject_NewPasswordReset_user_10 = NewPasswordReset (Right (Phone {fromPhone = "+41000616"}))
testObject_NewPasswordReset_user_11 :: NewPasswordReset
testObject_NewPasswordReset_user_11 = NewPasswordReset (Left (Email {emailLocal = "T[\CAN\1086972.n\DEL(~\136529\1027605\1076918\n+76", emailDomain = "<\CAN\EOT\DLEr\b1-&6D\STX\f\CANd_59"}))
testObject_NewPasswordReset_user_12 :: NewPasswordReset
testObject_NewPasswordReset_user_12 = NewPasswordReset (Right (Phone {fromPhone = "+04092488796984"}))
testObject_NewPasswordReset_user_13 :: NewPasswordReset
testObject_NewPasswordReset_user_13 = NewPasswordReset (Right (Phone {fromPhone = "+48530559853006"}))
testObject_NewPasswordReset_user_14 :: NewPasswordReset
testObject_NewPasswordReset_user_14 = NewPasswordReset (Left (Email {emailLocal = "dz\DC4NRb'\1041436`\22764\SI\133650s`\14591K\DC1w\SUB\156757m\194725_\NULS\NAK\":", emailDomain = "\156642E;f\148743\59296%\159635{75\1011252\170703\147132\NUL\DLE\ENQa\ENQ05"}))
testObject_NewPasswordReset_user_15 :: NewPasswordReset
testObject_NewPasswordReset_user_15 = NewPasswordReset (Right (Phone {fromPhone = "+3108298837512"}))
testObject_NewPasswordReset_user_16 :: NewPasswordReset
testObject_NewPasswordReset_user_16 = NewPasswordReset (Right (Phone {fromPhone = "+5162511449"}))
testObject_NewPasswordReset_user_17 :: NewPasswordReset
testObject_NewPasswordReset_user_17 = NewPasswordReset (Right (Phone {fromPhone = "+46453010268957"}))
testObject_NewPasswordReset_user_18 :: NewPasswordReset
testObject_NewPasswordReset_user_18 = NewPasswordReset (Left (Email {emailLocal = "U\nq\77898\STX\n$\ETB0X%", emailDomain = "\182530z\DC1\DLEa)\RS\b\n"}))
testObject_NewPasswordReset_user_19 :: NewPasswordReset
testObject_NewPasswordReset_user_19 = NewPasswordReset (Left (Email {emailLocal = "\1001875?(xg\ACK\128012'\136337\1001274\&7\DC2\EMe;", emailDomain = ",E\995602Hm\29888x\1105330g\23436\30113"}))
testObject_NewPasswordReset_user_20 :: NewPasswordReset
testObject_NewPasswordReset_user_20 = NewPasswordReset (Right (Phone {fromPhone = "+656606977153"}))
