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
testObject_ProviderActivationResponse_provider_1 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "w~\185192\EM_2\1027720CX\1068794]\1025033\1002939\ACK\"=M\137548\DEL\1048453", emailDomain = "\NUL_%\4864\f\ETXua[g\1001638\SI"}}
testObject_ProviderActivationResponse_provider_2 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_2 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "5T\EOT\ENQ\FS& \1001419e[\62164]", emailDomain = ""}}
testObject_ProviderActivationResponse_provider_3 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_3 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "d\171061\DC4\1018643\FSl\181367\"uhW\SIB'\1029965-/\f\STX", emailDomain = "s"}}
testObject_ProviderActivationResponse_provider_4 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_4 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "m\1036586\96428?\v\94043kJHr\44864\SYNoUqW|W\1018585Tvb/\40286u\1031166", emailDomain = "P3|;E`E"}}
testObject_ProviderActivationResponse_provider_5 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_5 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "", emailDomain = "lE%[&\1001590\37142\ETX)lHC\FS5"}}
testObject_ProviderActivationResponse_provider_6 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_6 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "Z\ACK\1109121\1011963\fF\171021T\180588!\DEL", emailDomain = "\ETX\DC3I\1080538\ACK\157590SSY=fD\f5\ETX^.\984564cF\FS"}}
testObject_ProviderActivationResponse_provider_7 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_7 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\DC1\1036199`", emailDomain = "lm\ETB\162240\7689gy~[N\162751A"}}
testObject_ProviderActivationResponse_provider_8 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_8 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\ESC\1019522\42815\&1Y\GSB", emailDomain = "i\1068878ku<\94315ZZ\DC1\ENQyQ\1054802\EM\\Z\153360\ENQ5\139883\SIn"}}
testObject_ProviderActivationResponse_provider_9 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_9 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\RS", emailDomain = "w\8302\a\139695\DC19=\11040\1083731\59500Jv,\STX:l!\ESC"}}
testObject_ProviderActivationResponse_provider_10 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_10 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\1059356N\1055518Z)\r,\EM\171441\70148el\52374:b`XF)y\157753=2\187178Z", emailDomain = "8\RS\1095176[K^"}}
testObject_ProviderActivationResponse_provider_11 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_11 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\ETB_", emailDomain = ""}}
testObject_ProviderActivationResponse_provider_12 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_12 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "K\1087664\SO(\872h\1052110\177412\1052231G_\156541I\63076\83142Y\aF\96932", emailDomain = "\ETX\DC3]&\993991hB2\v\94297\39478\&1\152358,[)\157299\DC4\1052068\&3\1001935\SOHL"}}
testObject_ProviderActivationResponse_provider_13 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_13 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "]\DC2F-\DEL+?x<\a\CAN1\USM\21732\ACK\rr", emailDomain = "\38776\DC4\1091291x`\1052846\EOT\128493\DELh2\170216\EOT_N\1100964c"}}
testObject_ProviderActivationResponse_provider_14 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_14 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "!>\b=.\137136\181026\1082779j{zK\985561JC>~{]kE\917832d-a", emailDomain = "v;\181531\1021951s"}}
testObject_ProviderActivationResponse_provider_15 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_15 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\166665\SOH\73872Y\ENQH\FS\1054630\r\tcj1bw", emailDomain = "|R9gL\ESC"}}
testObject_ProviderActivationResponse_provider_16 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_16 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "y\1089077\30731>!\1037040G\a\STX+m]PrNI \7175\&2\5312>V\n\1041210/jo", emailDomain = "\990758"}}
testObject_ProviderActivationResponse_provider_17 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_17 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\177490\1097742&\1071016\&0\1022309H\178814,\29285\&4\1055482uF\152417&\SUB(f\991537n{\1005891", emailDomain = "|\991415r\1037811\NUL\NUL=-w\tc\184890*"}}
testObject_ProviderActivationResponse_provider_18 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_18 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\SYN\SO\1101827\&2\99649\SUB\984958&%|\ENQ\ACK", emailDomain = "\n;\167906r\370\73942"}}
testObject_ProviderActivationResponse_provider_19 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_19 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "3C\STX\28387\29487\DEL\1055381K\1014336w\EM \987975u\1034104\a", emailDomain = "l\990522u'\155267%8\GS\180743\"\GS"}}
testObject_ProviderActivationResponse_provider_20 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_20 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "i\1105643l.=7\SYNX\1017717\EOTo\35094rE\fNXn)C\128711", emailDomain = "U,\1089457\94070B\1091461&oh\992962\FSxw\v\1083265\65715Yp\EM"}}
