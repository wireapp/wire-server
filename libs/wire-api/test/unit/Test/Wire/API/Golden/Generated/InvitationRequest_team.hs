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
testObject_InvitationRequest_team_1 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.TR, lCountry = Nothing}), irRole = Just RoleExternalPartner, irInviteeName = Just (Name {fromName = "\1019945r\156736\1028363\GS\150738\1101200i\SO5U?\61629p\EOT"}), irInviteeEmail = Email {emailLocal = "2`\n\21573?", emailDomain = "\DC4\SUB\1039055L^g"}, irInviteePhone = Nothing}
testObject_InvitationRequest_team_2 :: InvitationRequest
testObject_InvitationRequest_team_2 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.DE, lCountry = Just (Country {fromCountry = GD})}), irRole = Just RoleOwner, irInviteeName = Just (Name {fromName = "\1086322\998357\144433\39117BLA\997759\54131cQa\185179Y\NAKU\133131w_NT^\1065455\&8\a\170139IW\GSM\47112\&7GX\DC2b\"e8^;\1002632\135421@\14924\21632\140984\1059742\184107\36074\v\CAN\1102505=5t"}), irInviteeEmail = Email {emailLocal = "\FS8\1072004PRo", emailDomain = "\STX_"}, irInviteePhone = Just (Phone {fromPhone = "+0231420493"})}
testObject_InvitationRequest_team_3 :: InvitationRequest
testObject_InvitationRequest_team_3 = InvitationRequest {irLocale = Nothing, irRole = Nothing, irInviteeName = Just (Name {fromName = "\1027657\ETX#\SOH\156841\61333aZ\999650\984380\1084137@5G]c\SOH6^l?kM\999157W9\NULal'\RS\1026380\&4\DC1/\1074718\ENQrS`\t\25879(\183131\RS\144457\66784\984121%?\DC3H\36386G3X~\136054\GS'F\1170v\CANk\178180"}), irInviteeEmail = Email {emailLocal = "}\1007483#C#\"", emailDomain = "xA\1032791aX"}, irInviteePhone = Just (Phone {fromPhone = "+81784689367"})}
testObject_InvitationRequest_team_4 :: InvitationRequest
testObject_InvitationRequest_team_4 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.GN, lCountry = Just (Country {fromCountry = BM})}), irRole = Just RoleMember, irInviteeName = Just (Name {fromName = "9\ESC\22879\118883\25979\ETB\f8T%\1093622\14868\RSqoIlqy\1110585\1020473g\DC3\ENQF\r`\1062345-^Vpj\27527"}), irInviteeEmail = Email {emailLocal = "wK\1002473", emailDomain = ""}, irInviteePhone = Just (Phone {fromPhone = "+56382326604"})}
testObject_InvitationRequest_team_5 :: InvitationRequest
testObject_InvitationRequest_team_5 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.HY, lCountry = Just (Country {fromCountry = LV})}), irRole = Just RoleAdmin, irInviteeName = Just (Name {fromName = "\175602\142030u\GSj\\( W-l\6340"}), irInviteeEmail = Email {emailLocal = "h\1002107L", emailDomain = "\a"}, irInviteePhone = Just (Phone {fromPhone = "+073515410534"})}
