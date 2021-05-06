{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.EmailUpdate_user where

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
testObject_EmailUpdate_user_1 :: EmailUpdate
testObject_EmailUpdate_user_1 = EmailUpdate {euEmail = Email {emailLocal = "", emailDomain = "vA\FSC\1035499o\n\52529\DC3)"}}
testObject_EmailUpdate_user_2 :: EmailUpdate
testObject_EmailUpdate_user_2 = EmailUpdate {euEmail = Email {emailLocal = "p", emailDomain = "\24899Y,N.\1715\1078088\1069013*\1057712&"}}
testObject_EmailUpdate_user_3 :: EmailUpdate
testObject_EmailUpdate_user_3 = EmailUpdate {euEmail = Email {emailLocal = "X\STX\989955\1111005l:Fx-\1035374\DLE\984008\992111d\ETB0", emailDomain = "\158402\172983Vp\SUBC"}}
testObject_EmailUpdate_user_4 :: EmailUpdate
testObject_EmailUpdate_user_4 = EmailUpdate {euEmail = Email {emailLocal = "\STX>qT\DC1\EOTN\ENQ\46873\1022930\1059033\138449ee\f4U\1068161\26883\1078891h\149877\EM-\1027894", emailDomain = "*\1011888k}z4\175260\\\FS,\ACKr"}}
testObject_EmailUpdate_user_5 :: EmailUpdate
testObject_EmailUpdate_user_5 = EmailUpdate {euEmail = Email {emailLocal = "\f\992664ui\1014233=\STX\DC3H\168982D", emailDomain = "\3209X=\987584yBL*c\148630-D\v\399\r\SI\rw\1084063)\1047182Q\135035=/"}}
