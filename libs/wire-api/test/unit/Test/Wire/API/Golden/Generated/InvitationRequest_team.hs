{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.InvitationRequest_team where

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
testObject_InvitationRequest_team_1 :: InvitationRequest
testObject_InvitationRequest_team_1 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.AM, lCountry = Nothing}), irRole = Just RoleAdmin, irInviteeName = Just (Name {fromName = "\1036355aW\178724Bh\184557\ENQ4>o\57705[("}), irInviteeEmail = Email {emailLocal = "\GS\\\DC2jX", emailDomain = "\167920w\1082089\SUB\142231"}, irInviteePhone = Just (Phone {fromPhone = "+0767159103"})}
testObject_InvitationRequest_team_2 :: InvitationRequest
testObject_InvitationRequest_team_2 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SS, lCountry = Nothing}), irRole = Just RoleOwner, irInviteeName = Just (Name {fromName = "[\168179'\1016055M/yR\1064278ph\bg\165955\DC1L\tF\83164T\1059467\1070378\1051502\37462p<|*\DELi$\1021795W1\1056792\&7\DC3\34236Vb\1101470\SOHT\r\158060\1033501\1038890/\1005830\DLE\62370\&9\SOHy\1102522\149551\ETB1\ENQu\983117\983844g\60712\&7\tp\64705\8457j\NAKAL.\t>\SOH\f\a[#\1002420k4xJ\STX\147700>\SUBOq97\DC2\50526"}), irInviteeEmail = Email {emailLocal = "", emailDomain = ""}, irInviteePhone = Just (Phone {fromPhone = "+08093834436278"})}
testObject_InvitationRequest_team_3 :: InvitationRequest
testObject_InvitationRequest_team_3 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.ET, lCountry = Just (Country {fromCountry = NZ})}), irRole = Just RoleMember, irInviteeName = Nothing, irInviteeEmail = Email {emailLocal = "\985661]", emailDomain = "\DC4"}, irInviteePhone = Just (Phone {fromPhone = "+120845661566041"})}
testObject_InvitationRequest_team_4 :: InvitationRequest
testObject_InvitationRequest_team_4 = InvitationRequest {irLocale = Nothing, irRole = Just RoleMember, irInviteeName = Just (Name {fromName = "|\180362"}), irInviteeEmail = Email {emailLocal = "", emailDomain = "\60544"}, irInviteePhone = Just (Phone {fromPhone = "+106690427433878"})}
testObject_InvitationRequest_team_5 :: InvitationRequest
testObject_InvitationRequest_team_5 = InvitationRequest {irLocale = Nothing, irRole = Just RoleAdmin, irInviteeName = Just (Name {fromName = "J\\\171612\NAKE!w\22787&\1009841\94393/\a\SOHK}\FS[p\ENQG\SYN\1008614<\NAK6u\v\t\1066885Oj\r\988161\ENQ>\32377P\ak\DEL>\1068121nElCuz\NUL"}), irInviteeEmail = Email {emailLocal = "", emailDomain = "\159035"}, irInviteePhone = Just (Phone {fromPhone = "+839180700692701"})}
