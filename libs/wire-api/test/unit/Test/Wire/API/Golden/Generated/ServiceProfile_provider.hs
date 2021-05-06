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
testObject_ServiceProfile_provider_1 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))), serviceProfileName = Name {fromName = "\78786\n\78606~eu\v\149586F[[\1037050\NUL\1028558*G\DC4\11206\&4\59098$\DC4\1050292\5658\46240\44344\&8jT\59779qI\177394\ACK#\986184\987161d!68=\ACK\1103796\172602\&0-E\ESCcG.\DLE\n-\r-\STX\194622Om\1056808\EM|G\NAK/+\RSi_l\NAKF_\SUB,'P \SUB_n6I4z\\g@[B+SA\140876\62079Y\1069312d\74535D\991873H<o\t[\162345\987119\r:0aMEK\1052767\\'>Kf\36330"}, serviceProfileSummary = "", serviceProfileDescr = "\1102928\1054799\68482", serviceProfileAssets = [(ImageAsset "," (Just AssetComplete))], serviceProfileTags = fromList [GraphicsTag,WeatherTag], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_2 :: ServiceProfile
testObject_ServiceProfile_provider_2 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000"))), serviceProfileName = Name {fromName = "\DC2?\ETBo\194/TNz(\SYNQY\NAKm~\39760c$\1091415<\v?Q\NAK"}, serviceProfileSummary = "\DEL'\DEL", serviceProfileDescr = "\1069195", serviceProfileAssets = [(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview))], serviceProfileTags = fromList [SocialTag,SportsTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_3 :: ServiceProfile
testObject_ServiceProfile_provider_3 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002"))), serviceProfileName = Name {fromName = "<\SYNi:\178480\99892a\GS8\US\1102630\1029726\127960S=!X>cvnI|\155684\989984E0/e;v\SUB}d\1087392\n.\1043103\33315Z4 ]YW=\DLE2%emV4L\DLEg\DC4\1027248^Vf~\ETBn6~L\39235\&1|;9\CAN\US\NAK\159930\STX\ENQ;\r'\1062967-sgYb5e\1088723\48160\EOTXN\a\FS\165437\v\995860\1090694r\1017462\6789\SIA\1007626\171091*\186627\1812\FS\SUB\CANcCTbL\1092983(ah"}, serviceProfileSummary = "", serviceProfileDescr = "", serviceProfileAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview))], serviceProfileTags = fromList [], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_4 :: ServiceProfile
testObject_ServiceProfile_provider_4 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))), serviceProfileName = Name {fromName = "x\1054438i''J8H\1109318> \47289H\ETB\ETX\38207\DLEx\aP\17900;\US\38829cyN\1016951N\FSR\EMS:\n\135351\DLEk\172041\ETXSI\DLE)\143913\&7t\19887.\SOHEm?CZ\1016418\EOTpsv\DLEt\1069967\1017687\1041909w\1059631\NAK\STX_\1089827\60522\153360\74440ZrS\1084390\&0\n (\n\988616~\998847bI1\1109981\STX~"}, serviceProfileSummary = "\173255\DC2K", serviceProfileDescr = "", serviceProfileAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview))], serviceProfileTags = fromList [], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_5 :: ServiceProfile
testObject_ServiceProfile_provider_5 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000001"))), serviceProfileName = Name {fromName = "z&v\1047564\EOT3\151243m30RLT\194820\CAN2E\1056230\1064061\1097203Z\"d\ACK\CAN\aFX\1083436}r\ETXr\54062\NAK>\118837&\EM\177465\ACK\SOHM+@\GS\42256\t\EM\DC3z0\\"}, serviceProfileSummary = "", serviceProfileDescr = "", serviceProfileAssets = [], serviceProfileTags = fromList [MedicalTag], serviceProfileEnabled = False}
