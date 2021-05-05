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
testObject_InvitationRequest_1 :: InvitationRequest
testObject_InvitationRequest_1 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.FY, lCountry = Nothing}), irRole = Nothing, irInviteeName = Just (Name {fromName = "A\"\161132\140308;\71125\187437\1014809s$t\165686\1032942B\vC/0W\nA{n\23556VZg\"6\1088708r,A{\1080192\FS\1042344\171646z\ETX_\33082\ab\95807\FS\nf \USC\n\DC2e0m\ESC4"}), irInviteeEmail = Email {emailLocal = "", emailDomain = "\1057804M"}, irInviteePhone = Just (Phone {fromPhone = "+3642341107"})}
testObject_InvitationRequest_2 :: InvitationRequest
testObject_InvitationRequest_2 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.LT, lCountry = Just (Country {fromCountry = BF})}), irRole = Just RoleAdmin, irInviteeName = Just (Name {fromName = "Hy\189066L"}), irInviteeEmail = Email {emailLocal = "U", emailDomain = "\143586\164861b\SUB\\2"}, irInviteePhone = Just (Phone {fromPhone = "+51521265277"})}
testObject_InvitationRequest_3 :: InvitationRequest
testObject_InvitationRequest_3 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.FA, lCountry = Just (Country {fromCountry = DO})}), irRole = Just RoleOwner, irInviteeName = Just (Name {fromName = "T\ENQ\1061397Z-;\vxn\141921\52328\ESC;;\r\EOT\134496`H\CANS\8555x\SOH\1069416C\1086014\142671\EM\1105788<>\SYN:\n\1822-\158296\DC18jq\1058299z\156662e5\26664\53201G\ENQ\DLEJ\9447>0\ENQq7 \ETB\120955..\ESC2j\DEL\93988\NAK\ETX-\1010012\DC2\1057542#p1\64572dylH+_\GS\1076045O\46646\f\FS]+\EM\1064042\&0"}), irInviteeEmail = Email {emailLocal = "\\\SUB\177126\CAN", emailDomain = "\NUL\1088450\SUBS\US\SI"}, irInviteePhone = Just (Phone {fromPhone = "+81882365947"})}
testObject_InvitationRequest_4 :: InvitationRequest
testObject_InvitationRequest_4 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.BH, lCountry = Nothing}), irRole = Nothing, irInviteeName = Nothing, irInviteeEmail = Email {emailLocal = ">\DC2\100231#", emailDomain = "Q\144564\24502"}, irInviteePhone = Just (Phone {fromPhone = "+97949231859369"})}
testObject_InvitationRequest_5 :: InvitationRequest
testObject_InvitationRequest_5 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.JA, lCountry = Just (Country {fromCountry = KG})}), irRole = Nothing, irInviteeName = Nothing, irInviteeEmail = Email {emailLocal = "CZ", emailDomain = "W#\1084147\&9\f\DC3"}, irInviteePhone = Nothing}
