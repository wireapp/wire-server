{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Event_team where

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
import Data.LegalHold
import Wire.API.Conversation.Role
import Wire.API.Event.Team
import Wire.API.Provider.Service
import Wire.API.Team
import Wire.API.Team.Conversation
import Wire.API.Team.Feature
import Wire.API.Team.Invitation
import Wire.API.Team.LegalHold
import Wire.API.Team.LegalHold.External
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.Team.Role
import Wire.API.Team.SearchVisibility
import Wire.API.User.Client.Prekey
import Wire.API.User.Identity
import Wire.API.User.Profile
testObject_Event_1 :: Event
testObject_Event_1 = (newEvent (TeamCreate) ((Id (fromJust (UUID.fromString "00006690-0000-7538-0000-1191000022dd")))) (read ("1864-05-01 07:15:32.391560213494 UTC")) & eventData .~ (Just (EdTeamCreate (newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000400000002")))) ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000400000000")))) ("") ("\a") (Binding) & teamIconKey .~ (Just "J\1001920")))))
testObject_Event_2 :: Event
testObject_Event_2 = (newEvent (TeamDelete) ((Id (fromJust (UUID.fromString "000052b1-0000-53ef-0000-736f00007260")))) (read ("1864-05-23 05:11:26.695869554801 UTC")) & eventData .~ (Nothing))
testObject_Event_3 :: Event
testObject_Event_3 = (newEvent (ConvDelete) ((Id (fromJust (UUID.fromString "0000116f-0000-68fd-0000-1c1500004a73")))) (read ("1864-05-14 05:14:41.067947983243 UTC")) & eventData .~ (Just (EdConvDelete (Id (fromJust (UUID.fromString "00003b32-0000-2b6c-0000-2e1a000005a0"))))))
testObject_Event_4 :: Event
testObject_Event_4 = (newEvent (MemberUpdate) ((Id (fromJust (UUID.fromString "00003c89-0000-5277-0000-60d400003666")))) (read ("1864-04-17 16:11:18.251532774535 UTC")) & eventData .~ (Just (EdMemberUpdate (Id (fromJust (UUID.fromString "0000193f-0000-6993-0000-683e000001af"))) (Just (Permissions {_self = fromList [CreateConversation,RemoveTeamMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetBilling,GetMemberPermissions,DeleteTeam], _copy = fromList [CreateConversation,DoNotUseDeprecatedModifyConvName,GetBilling,SetBilling,GetMemberPermissions,DeleteTeam]})))))
testObject_Event_5 :: Event
testObject_Event_5 = (newEvent (ConvCreate) ((Id (fromJust (UUID.fromString "0000390f-0000-4ab1-0000-3dc40000317d")))) (read ("1864-06-01 21:32:53.152920463166 UTC")) & eventData .~ (Just (EdConvCreate (Id (fromJust (UUID.fromString "00005ced-0000-4298-0000-27e30000714c"))))))
