{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ServiceProfile_provider where

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
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Provider
import Wire.API.Provider.Bot
import Wire.API.Provider.External
import Wire.API.Provider.Service
import Wire.API.Provider.Service.Tag
import Wire.API.User.Client.Prekey
import Wire.API.User.Identity
import Wire.API.User.Profile
testObject_ServiceProfile_provider_1 :: ServiceProfile
testObject_ServiceProfile_provider_1 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000"))), serviceProfileName = Name {fromName = "\DC4\1108657\SUB\1054058\GSBtcG}4t\SOH\STXR]\1070641\3545\179654\1104954c\134466/\57399A\991484t\43326\SOH\"cF_\118877g"}, serviceProfileSummary = "\1018695\&8B", serviceProfileDescr = "\179823\\\146368", serviceProfileAssets = [], serviceProfileTags = fromList [RatingTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_2 :: ServiceProfile
testObject_ServiceProfile_provider_2 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002"))), serviceProfileName = Name {fromName = "\ETX\1044535w\1049241!\137525\SOHKI \94325\68063#dd\SO4\1049565\167726iA/\143195\&57\rh0\1105522{\168965o\25866\\\1084788oM\al"}, serviceProfileSummary = "", serviceProfileDescr = "H,", serviceProfileAssets = [], serviceProfileTags = fromList [FitnessTag,QuizTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_3 :: ServiceProfile
testObject_ServiceProfile_provider_3 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))), serviceProfileName = Name {fromName = "B\1082349\US-\174007O\DLE]\SUB\tV\992887\SOH$YV'0\bl \593`GS95j\CANb\19085j9s\1067626\&4\64923J*>d[X\SI?\NAK\180659\1025479f&\US&L\RS\34713}\1112440Z\65014\21687\118790\STXY\n\ENQ<\DC1\181575\ACK\1045802ch|m5\GSTfI8\f,itXpAkt:\1095975E\988800EEu$\1023528[V\60550\RS\ENQ\f\39185\EM{\6970i0z\1095063\43652\r\ACK\1036572\161358A\SOHCH\RSC\ETB~(\146425"}, serviceProfileSummary = ",", serviceProfileDescr = "", serviceProfileAssets = [], serviceProfileTags = fromList [], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_4 :: ServiceProfile
testObject_ServiceProfile_provider_4 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))), serviceProfileName = Name {fromName = "j%\27531\SI\1094277\&4odr@9\SOHJ\157338\100115\988377\168872"}, serviceProfileSummary = "", serviceProfileDescr = "\ENQ", serviceProfileAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete))], serviceProfileTags = fromList [], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_5 :: ServiceProfile
testObject_ServiceProfile_provider_5 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001"))), serviceProfileName = Name {fromName = "V\a&8>B\1019126\vr+B2yzY\987695\194644\ACKzh\US+\989961\78890\1022055\DC4\145772!v\tZ\DC1\18095\b\DC4=}\96854Y,%\66007\177180\&1n\1052649\SYN[\141108\36107$\ESCR\a\150089\27557\47052\FS=\9249<\1084570lz\128565r\57604\DC3\31469\157734\&5Wq\DC2mFvDG\1014600.\119006\SUBor568\DELm#1\SI\SUB]"}, serviceProfileSummary = "k\NAK\1068229", serviceProfileDescr = "u\68779", serviceProfileAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete))], serviceProfileTags = fromList [MedicalTag,SportsTag], serviceProfileEnabled = True}
