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
testObject_ServiceProfile_1 :: ServiceProfile
testObject_ServiceProfile_1 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), serviceProfileName = Name {fromName = "^\141220ktJ^'n@'e\37450\1000602_\142370$\43074\172327\169171.Lx\DLE\ESC\1037286\8452<\ETX\ENQ)$~]\a\1075206\1094717\169094\69217j3V\170957\1055882\989539\70272LX\1053602\1014093\1039491&'^\38915zd#\1026770"}, serviceProfileSummary = "lr", serviceProfileDescr = "\996821@{", serviceProfileAssets = [(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Nothing))], serviceProfileTags = fromList [GamesTag], serviceProfileEnabled = True}
testObject_ServiceProfile_2 :: ServiceProfile
testObject_ServiceProfile_2 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000000"))), serviceProfileName = Name {fromName = "0\1063510#\DC2\10175\66385K\n:\NAK\1014279\128756.\1084732\68380\1002235\35166^\1083039\31723\r\DC3\23378hv\14879\RSd\1008254O\28249>D\148026:r\ETBZ)\1042161\&8\1049154\STX\1038131\&3\1103440ro/\1044140w\STXi2\1039565\1029968\f\EOT\33592\EM\\>\ETB\ETX\ETXV\1097239\"I\\.#+F\f2H\93981\ACKA7\NAK~\1005309\1084026\a\49168}[9\1039501\DC2\58383\"\181543\&5\168760w\RS-\rS^"}, serviceProfileSummary = "\12700D\996476", serviceProfileDescr = "s", serviceProfileAssets = [(ImageAsset "x" (Just AssetComplete))], serviceProfileTags = fromList [MedicalTag,PhotographyTag,TutorialTag], serviceProfileEnabled = False}
testObject_ServiceProfile_3 :: ServiceProfile
testObject_ServiceProfile_3 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))), serviceProfileName = Name {fromName = ">\ETXSV&M\191438\999569\r\FS\DEL\DC1"}, serviceProfileSummary = "", serviceProfileDescr = "Z9", serviceProfileAssets = [(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete))], serviceProfileTags = fromList [], serviceProfileEnabled = False}
testObject_ServiceProfile_4 :: ServiceProfile
testObject_ServiceProfile_4 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000002"))), serviceProfileName = Name {fromName = "5,\SYNa%\13848j\177995<r]B\tu\39989\EOTS\GS$%\ENQN\37805\STXKEttet@\DC3\DC4\1090835"}, serviceProfileSummary = "\29946\SUB", serviceProfileDescr = "@", serviceProfileAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview))], serviceProfileTags = fromList [BusinessTag], serviceProfileEnabled = False}
testObject_ServiceProfile_5 :: ServiceProfile
testObject_ServiceProfile_5 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))), serviceProfileName = Name {fromName = "P"}, serviceProfileSummary = "\59986", serviceProfileDescr = "[", serviceProfileAssets = [], serviceProfileTags = fromList [LifestyleTag], serviceProfileEnabled = True}
