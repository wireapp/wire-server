{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ProviderActivationResponse_provider where

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
testObject_ProviderActivationResponse_provider_1 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_1 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\184602E", emailDomain = "\147345x0S\1032598\SOH2{\1101610-\DC4\SUB:\1019772\DC1\156047\1048548+Y\144303"}}
testObject_ProviderActivationResponse_provider_2 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_2 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\DELF\CAN\171014}yu\1032857*\54195!YU\133980{\ETB\1021188-", emailDomain = "_\a}J0`\SUB"}}
testObject_ProviderActivationResponse_provider_3 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_3 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "Z\ETB'\137489\53238k3\DEL#\415\ACK\1076950\ETBs\171693\100116i{\vz\EOT3\"g\STXL ,p\32964", emailDomain = "tL\ESCu< \1028579c4\ACKa\1050882\98389m\35883\169278\ENQ\EM\142250Dfwb6,\DC2%B"}}
testObject_ProviderActivationResponse_provider_4 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_4 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "(N|[oN\SYN7\1010947b\SI>{\13340\&8t", emailDomain = "\133746+y\2351V%e}<s\1067948>b\1046979\26120u\v\EMOr"}}
testObject_ProviderActivationResponse_provider_5 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_5 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "?\b\123618\f\158985~b\1074573k~", emailDomain = "\CAN?\1064860Z=H8"}}
