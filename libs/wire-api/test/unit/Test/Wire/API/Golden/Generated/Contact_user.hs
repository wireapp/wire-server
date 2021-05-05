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
testObject_Contact_1 :: Contact
testObject_Contact_1 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000003-0000-0008-0000-000400000002"))), qDomain = Domain {_domainText = "8.o-9"}}, contactName = "h\1038227u\DELp", contactColorId = Just 1, contactHandle = Just "l\US_A", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0008-0000-000800000006")))}
testObject_Contact_2 :: Contact
testObject_Contact_2 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000200000006"))), qDomain = Domain {_domainText = "y8nd.sd-nf-5a4lkt7"}}, contactName = "\SI", contactColorId = Just (-3), contactHandle = Just "\99986\179267i", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000600000007")))}
testObject_Contact_3 :: Contact
testObject_Contact_3 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000008-0000-0006-0000-000500000000"))), qDomain = Domain {_domainText = "6-59.oo.k"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "\b\DC4", contactTeam = Just (Id (fromJust (UUID.fromString "00000005-0000-0008-0000-000600000002")))}
testObject_Contact_4 :: Contact
testObject_Contact_4 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000007-0000-0007-0000-000700000006"))), qDomain = Domain {_domainText = "6-83.9-j.sc1j1"}}, contactName = "\169848\1058698", contactColorId = Just (-1), contactHandle = Just "iW\1009308\SIO", contactTeam = Just (Id (fromJust (UUID.fromString "00000007-0000-0004-0000-000300000003")))}
testObject_Contact_5 :: Contact
testObject_Contact_5 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000006-0000-0006-0000-000100000001"))), qDomain = Domain {_domainText = "gf6.i70"}}, contactName = "", contactColorId = Just 1, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000005-0000-0008-0000-000000000002")))}
