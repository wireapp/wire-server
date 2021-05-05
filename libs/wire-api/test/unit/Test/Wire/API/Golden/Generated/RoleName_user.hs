{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.RoleName_user where

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
testObject_RoleName_1 :: RoleName
testObject_RoleName_1 = (fromJust (parseRoleName "b6cywaaxbimm0b6dbozbmkle8admwhavikz0qu914vmkwvid01pq2ob8er2f5wgz0919b2xeb3ntn92a7lt6stviko6"))
testObject_RoleName_2 :: RoleName
testObject_RoleName_2 = (fromJust (parseRoleName "br2w9_60_xavld1v834rzd_pxho4eiu35hfst7min6pgyjhxuvr6jtomv8mcfq304f8se07ct4n0ztoiqdpqhlafc53ga910ru1aqu6230lw_mg7yw5uy8hk0q"))
testObject_RoleName_3 :: RoleName
testObject_RoleName_3 = (fromJust (parseRoleName "yf8t2fwxmcp3zlj4ox9lvsrsicvamotewiz14b9g3gl8pe_r4dcf7hg302ft0omt_xs1g1rarj5ngyo27az0tdye0tam5nat6ogm"))
testObject_RoleName_4 :: RoleName
testObject_RoleName_4 = (fromJust (parseRoleName "0pjp3fyhpl_e4mhfs2lwsi8956ev42qlk5p1ted_q6eiriagvq5g"))
testObject_RoleName_5 :: RoleName
testObject_RoleName_5 = (fromJust (parseRoleName "buiia7wcctnof91n9ugrgw5iatshoxkzjqokfxk4kfby147q05bhka8l_pnlp_okq4nyiy058sde6aw_yxpouoy8zm3gbgkgzddjwamt1lxjxr3k1z"))
