{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Contact_user where

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
testObject_Contact_user_1 :: Contact
testObject_Contact_user_1 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0007-0000-000700000003"))), qDomain = Domain {_domainText = "7b49i05vn-g4d93--w.lx7c.xerb.a"}}, contactName = "8k\1058s\DC1m", contactColorId = Just (-5), contactHandle = Just "\176971XN~7\1018472", contactTeam = Just (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000400000002")))}
testObject_Contact_user_2 :: Contact
testObject_Contact_user_2 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000005"))), qDomain = Domain {_domainText = "qr.acdu4"}}, contactName = "Rq\190734\1098461\t", contactColorId = Just 4, contactHandle = Just "cTz(\1039584U", contactTeam = Nothing}
testObject_Contact_user_3 :: Contact
testObject_Contact_user_3 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000004-0000-0006-0000-000500000001"))), qDomain = Domain {_domainText = "2y.to3e.p7-90pd"}}, contactName = "", contactColorId = Nothing, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0008-0000-000700000004")))}
testObject_Contact_user_4 :: Contact
testObject_Contact_user_4 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000006-0000-0002-0000-000800000006"))), qDomain = Domain {_domainText = "330gp71p4-mt0914l52.p"}}, contactName = "", contactColorId = Just 0, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000400000001")))}
testObject_Contact_user_5 :: Contact
testObject_Contact_user_5 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000005-0000-0008-0000-000800000008"))), qDomain = Domain {_domainText = "ovz-lby-q90-bt.04273zd-b.u"}}, contactName = "\1016509\DC1", contactColorId = Nothing, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0006-0000-000600000006")))}
