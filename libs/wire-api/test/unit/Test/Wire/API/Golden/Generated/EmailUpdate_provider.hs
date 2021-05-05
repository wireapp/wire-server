{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.EmailUpdate_provider where

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
testObject_EmailUpdate_1 :: EmailUpdate
testObject_EmailUpdate_1 = EmailUpdate {euEmail = Email {emailLocal = "$g\16212\ACKM6\USA", emailDomain = "\1086187+\136305\NULh\3666\54447\&0d4\44839wE\44459\SOH\\\149877s[k\DEL"}}
testObject_EmailUpdate_2 :: EmailUpdate
testObject_EmailUpdate_2 = EmailUpdate {euEmail = Email {emailLocal = "d\1024437,R\"\1027966H\DC1zCQ\1006753\ENQh\60963mvY\29848\1071002", emailDomain = "46\1020463\4455\"Bwg<\119664J\28059\EOT\52625W\998468PccA\v|4"}}
testObject_EmailUpdate_3 :: EmailUpdate
testObject_EmailUpdate_3 = EmailUpdate {euEmail = Email {emailLocal = ")&Z)", emailDomain = "\11111~0Sb\162357\SUB\58110\n\EOTBPY\1000231+"}}
testObject_EmailUpdate_4 :: EmailUpdate
testObject_EmailUpdate_4 = EmailUpdate {euEmail = Email {emailLocal = "C\158693\66460\11688v\1006323L\38735a\GS|W\35706\NUL", emailDomain = "q\1078454mYG\v\1076830\ETB"}}
testObject_EmailUpdate_5 :: EmailUpdate
testObject_EmailUpdate_5 = EmailUpdate {euEmail = Email {emailLocal = "\NAK\1066596\1048628)\983403Gk", emailDomain = "k4U\NAKNV/\1878M"}}
