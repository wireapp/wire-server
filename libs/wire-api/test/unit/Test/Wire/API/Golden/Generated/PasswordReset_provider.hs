{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.PasswordReset_provider where

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
testObject_PasswordReset_1 :: PasswordReset
testObject_PasswordReset_1 = PasswordReset {nprEmail = Email {emailLocal = "\1000302,", emailDomain = "6=\141129\187449f}.(t[CL\1055130\DC1:v/)#m\997965\169187X*F4d\40341"}}
testObject_PasswordReset_2 :: PasswordReset
testObject_PasswordReset_2 = PasswordReset {nprEmail = Email {emailLocal = "\1050252f\f\1104281\1039619V\1104903\SI\169608p\987412$\189382\CAN\DC2?\168689o\1012431\DC4\4994\b\1885\NAKPzC\DC4", emailDomain = "P \DC3\1020065\SYN\994470}\FS\SO&9\SYNX#!\1016108\1080767"}}
testObject_PasswordReset_3 :: PasswordReset
testObject_PasswordReset_3 = PasswordReset {nprEmail = Email {emailLocal = "F\SO43\1075247O\a\NUL<\57605K\1103309\1025781\STXz\RS\1102629\1016240", emailDomain = "Y\173254\1086989\124991\160291\&7\7385\vz\STXe\25278Z{YO\NAK\f\1063863n\\zE\1035765vE\150019\1096395"}}
testObject_PasswordReset_4 :: PasswordReset
testObject_PasswordReset_4 = PasswordReset {nprEmail = Email {emailLocal = "\SO d\1112958$\1014601tt{T\1113791\180109J", emailDomain = " "}}
testObject_PasswordReset_5 :: PasswordReset
testObject_PasswordReset_5 = PasswordReset {nprEmail = Email {emailLocal = "#]\1109768|\133239dv1f\\G\156321\&2^", emailDomain = "\ENQ\SI\ac\1058464\52283-p|w\1001972]$\r!1a{\ETX\1019073H\1093646\tb0\1059427\988210\NULd"}}
