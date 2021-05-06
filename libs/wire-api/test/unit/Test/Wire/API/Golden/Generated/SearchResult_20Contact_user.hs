{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.SearchResult_20Contact_user where

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
testObject_SearchResult_20Contact_user_1 :: SearchResult Contact
testObject_SearchResult_20Contact_user_1 = SearchResult {searchFound = -2, searchReturned = -7, searchTook = 6, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), qDomain = Domain {_domainText = "04.3w35.u2q"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Nothing},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), qDomain = Domain {_domainText = "5b942eh1x.s34"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))}]}
testObject_SearchResult_20Contact_user_2 :: SearchResult Contact
testObject_SearchResult_20Contact_user_2 = SearchResult {searchFound = -6, searchReturned = -2, searchTook = 3, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), qDomain = Domain {_domainText = "gs-s9c3.bm059f0-6--p8.zf7dg.u-13ifm.19-we.s-e"}}, contactName = "", contactColorId = Nothing, contactHandle = Nothing, contactTeam = Nothing},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), qDomain = Domain {_domainText = "64spsi004.5-6i9u18.g3q9.p69o-a215-i43"}}, contactName = "", contactColorId = Just 0, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))}]}
testObject_SearchResult_20Contact_user_3 :: SearchResult Contact
testObject_SearchResult_20Contact_user_3 = SearchResult {searchFound = 4, searchReturned = -1, searchTook = 4, searchResults = []}
testObject_SearchResult_20Contact_user_4 :: SearchResult Contact
testObject_SearchResult_20Contact_user_4 = SearchResult {searchFound = 7, searchReturned = 2, searchTook = 7, searchResults = []}
testObject_SearchResult_20Contact_user_5 :: SearchResult Contact
testObject_SearchResult_20Contact_user_5 = SearchResult {searchFound = 6, searchReturned = 0, searchTook = 5, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), qDomain = Domain {_domainText = "86-t1o63wqw-1c3bw757.8t.337.hdj"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), qDomain = Domain {_domainText = "9p-71t8.7m-3g.05.t-0ga--z61375-6"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), qDomain = Domain {_domainText = "9-21.pddi14.8g4.lf.ne39.dw2"}}, contactName = "", contactColorId = Just 0, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), qDomain = Domain {_domainText = "1f.jb-y.k-07767-5o"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), qDomain = Domain {_domainText = "iqm9v.er5.q"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), qDomain = Domain {_domainText = "e-mp.h-39u9"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), qDomain = Domain {_domainText = "zhffl-rvq--3.r6-778iv10d.p8hl.x"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))}]}
