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
testObject_EmailUpdate_provider_1 :: EmailUpdate
testObject_EmailUpdate_provider_1 = EmailUpdate {euEmail = Email {emailLocal = "\b\"$\1081641\1056671XNkq\a=\FS\STXM\127541\1040627MI3~\1031582\ESC?^", emailDomain = "oA^]\f\a"}}
testObject_EmailUpdate_provider_2 :: EmailUpdate
testObject_EmailUpdate_provider_2 = EmailUpdate {euEmail = Email {emailLocal = "\NUL*\174613\ETB\68472\a'j3\FS,\132780i", emailDomain = "\178958\bX\"\ETB\1036160]j]+\1050215^\v\25802\b\986241\50243n\n{\DEL"}}
testObject_EmailUpdate_provider_3 :: EmailUpdate
testObject_EmailUpdate_provider_3 = EmailUpdate {euEmail = Email {emailLocal = "\SOH\27781u5a{\1019941I\185990\94910]JH\136295!\SUB\181696\1111092\150661\1023295\1024493", emailDomain = "'}<\bPr?C\14079\43036U:\EMEzm\60106!\f\1071850\&5Su{\1109318U\126232\1066377Z"}}
testObject_EmailUpdate_provider_4 :: EmailUpdate
testObject_EmailUpdate_provider_4 = EmailUpdate {euEmail = Email {emailLocal = "qb\1046935}`\SOH\f\1021361Y\DC4\ACK\1086328", emailDomain = "\1023791\&4q1a\7746\11234\1369o\\\1082105\FS\10909\&3b3\1070583b\9894\t?m"}}
testObject_EmailUpdate_provider_5 :: EmailUpdate
testObject_EmailUpdate_provider_5 = EmailUpdate {euEmail = Email {emailLocal = "m\DC1Nr`", emailDomain = "\35662"}}
testObject_EmailUpdate_provider_6 :: EmailUpdate
testObject_EmailUpdate_provider_6 = EmailUpdate {euEmail = Email {emailLocal = "B7\63723\SI", emailDomain = "\1004723\&1\"\2891\92300"}}
testObject_EmailUpdate_provider_7 :: EmailUpdate
testObject_EmailUpdate_provider_7 = EmailUpdate {euEmail = Email {emailLocal = "\1109713&\12700:A\DLE'\1067426N\\\CAN", emailDomain = "8wC\136398\SOR{O1#q*_^\58795\CANfa\bE\SUB\1030316\62251"}}
testObject_EmailUpdate_provider_8 :: EmailUpdate
testObject_EmailUpdate_provider_8 = EmailUpdate {euEmail = Email {emailLocal = "?\1112005\7692T(\EOT\1029487\FS2.5\161522", emailDomain = "BW\NAK\183299\51603\148605J\27035$\NUL~\47299\183648D\SYN\49790\1096309dP0\1012652U\1003146lS"}}
testObject_EmailUpdate_provider_9 :: EmailUpdate
testObject_EmailUpdate_provider_9 = EmailUpdate {euEmail = Email {emailLocal = "`-t&\1073718\994951\DLE\SUB\25446\&1?\1028658_\b\"\1065502\1026970\&8dRpV\917624>\136642B\1058236", emailDomain = "\SOH+7$74\127464\163140k`\171910v8\25226I~\1111732!\988414-C\1054886\DEL\22428\EOT"}}
testObject_EmailUpdate_provider_10 :: EmailUpdate
testObject_EmailUpdate_provider_10 = EmailUpdate {euEmail = Email {emailLocal = "\DC1\SUB\DC2\1013820Qcz3\1031271\96294Ui[.q\1070348<\51892K\GS\1054844'SC\SI\"", emailDomain = "}"}}
testObject_EmailUpdate_provider_11 :: EmailUpdate
testObject_EmailUpdate_provider_11 = EmailUpdate {euEmail = Email {emailLocal = "\SUB#W!%", emailDomain = "\1020736\&5R"}}
testObject_EmailUpdate_provider_12 :: EmailUpdate
testObject_EmailUpdate_provider_12 = EmailUpdate {euEmail = Email {emailLocal = "", emailDomain = "k\r\f\1020888}_)\8530r\139131\NAK\58637`}"}}
testObject_EmailUpdate_provider_13 :: EmailUpdate
testObject_EmailUpdate_provider_13 = EmailUpdate {euEmail = Email {emailLocal = "rUJo\11722\62295\STXY\1109158k\":\ETX\DC1.b\43923\b\1028942\52247\37028\157024\&5", emailDomain = "\GSV\1012063?\ENQe\161011Olj\22436]\1081259\65537\&2\985952\ETB"}}
testObject_EmailUpdate_provider_14 :: EmailUpdate
testObject_EmailUpdate_provider_14 = EmailUpdate {euEmail = Email {emailLocal = "\ETX3zToz\ACKK\48844$", emailDomain = "\SYNGB."}}
testObject_EmailUpdate_provider_15 :: EmailUpdate
testObject_EmailUpdate_provider_15 = EmailUpdate {euEmail = Email {emailLocal = "]\993936'\1069997\DEL\1113408JV\1083642Ayux9o+\\Y<[\EM", emailDomain = "\53746W|b\1099365\DC1P\nl\n"}}
testObject_EmailUpdate_provider_16 :: EmailUpdate
testObject_EmailUpdate_provider_16 = EmailUpdate {euEmail = Email {emailLocal = "<\166006_Nt3t\1016744o>2\EM>\45468\997984\SIM n\vM\188369\r", emailDomain = "`aw$\ENQe>z"}}
testObject_EmailUpdate_provider_17 :: EmailUpdate
testObject_EmailUpdate_provider_17 = EmailUpdate {euEmail = Email {emailLocal = "\1047067KJ:\GS\ESCV\134184C0B\1043491\172462\FS4#V|S2&\ETBy\EOT\155344\&3!AJ", emailDomain = "M\ESC\SO!0t\195022B"}}
testObject_EmailUpdate_provider_18 :: EmailUpdate
testObject_EmailUpdate_provider_18 = EmailUpdate {euEmail = Email {emailLocal = "X\DC19\ETB\b\152310\1091362>\16292[", emailDomain = "9FW\126983i\991364r\137256'\FST\fU"}}
testObject_EmailUpdate_provider_19 :: EmailUpdate
testObject_EmailUpdate_provider_19 = EmailUpdate {euEmail = Email {emailLocal = ".5\24380\35735>M\SI\139251\DEL\ENQ$\96766,\nj'GA\v", emailDomain = "\n\180161\DC2\1005415tRRLl\tj\120609\1000770"}}
testObject_EmailUpdate_provider_20 :: EmailUpdate
testObject_EmailUpdate_provider_20 = EmailUpdate {euEmail = Email {emailLocal = "\26180qRZ\USW\"$x\157395\&5Z;;V\r\SI\1094058\991679\&3Nh\1070662l\1027759", emailDomain = "*M\16748\US\19196\1042557-rl\RS\n\EOT*B\1110234"}}
