{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Team_team where

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
testObject_Team_team_1 :: Team
testObject_Team_team_1 = (newTeam ((Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000200000004")))) ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002")))) ("") ("1.9`r") (NonBinding) & teamIconKey .~ (Just "i\147832"))
testObject_Team_team_2 :: Team
testObject_Team_team_2 = (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000200000000")))) ((Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000200000001")))) ("\1025205") ("j\GS\n\1108753\1078125") (NonBinding) & teamIconKey .~ (Just "Z"))
testObject_Team_team_3 :: Team
testObject_Team_team_3 = (newTeam ((Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000400000003")))) ("}\1106180\152349d") ("Orl\SYNp") (Binding) & teamIconKey .~ (Just "{9Y"))
testObject_Team_team_4 :: Team
testObject_Team_team_4 = (newTeam ((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000100000004")))) ((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000300000003")))) ("\SUB\r\RS\146508") ("}*\1006745") (NonBinding) & teamIconKey .~ (Nothing))
testObject_Team_team_5 :: Team
testObject_Team_team_5 = (newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000200000002")))) ((Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000400000002")))) ("") ("\60106") (NonBinding) & teamIconKey .~ (Just "n\CAN"))
testObject_Team_team_6 :: Team
testObject_Team_team_6 = (newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000400000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000002")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "\DC4"))
testObject_Team_team_7 :: Team
testObject_Team_team_7 = (newTeam ((Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000400000001")))) ((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000400000004")))) ("{\1058185r\725") ("VQ") (NonBinding) & teamIconKey .~ (Just "(\STX\1020381\ENQ"))
testObject_Team_team_8 :: Team
testObject_Team_team_8 = (newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000004")))) ((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000000000003")))) ("\b\154354") ("\66612\&8\1030967b\DEL") (Binding) & teamIconKey .~ (Just ""))
testObject_Team_team_9 :: Team
testObject_Team_team_9 = (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000000000004")))) ("") ("6\DC1Lij") (NonBinding) & teamIconKey .~ (Just "t"))
testObject_Team_team_10 :: Team
testObject_Team_team_10 = (newTeam ((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000000000002")))) ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000001")))) ("\1038222\1020242$") ("i") (Binding) & teamIconKey .~ (Just ""))
testObject_Team_team_11 :: Team
testObject_Team_team_11 = (newTeam ((Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000400000002")))) ((Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000300000000")))) ("\1091261\n\US\SYNC") ("\NAK\1113745\20039\DC3e") (NonBinding) & teamIconKey .~ (Just "\16548.\1094077\&9"))
testObject_Team_team_12 :: Team
testObject_Team_team_12 = (newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000400000002")))) ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000300000000")))) (".yHo") ("x\1036961") (NonBinding) & teamIconKey .~ (Just "g\nC\1049158\NUL"))
testObject_Team_team_13 :: Team
testObject_Team_team_13 = (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000400000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000400000001")))) ("") ("") (Binding) & teamIconKey .~ (Just "|\DLEx\996154"))
testObject_Team_team_14 :: Team
testObject_Team_team_14 = (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000200000003")))) ((Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000200000001")))) ("+s \NAK?") ("HbL\65156/") (Binding) & teamIconKey .~ (Just "\188656\1042403"))
testObject_Team_team_15 :: Team
testObject_Team_team_15 = (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000002")))) ((Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000400000004")))) ("-\1059138") ("") (Binding) & teamIconKey .~ (Just "\NUL_4@"))
testObject_Team_team_16 :: Team
testObject_Team_team_16 = (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000000000004")))) ((Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000300000001")))) ("z") ("r\ACKU\rF") (Binding) & teamIconKey .~ (Just "oU"))
testObject_Team_team_17 :: Team
testObject_Team_team_17 = (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000400000003")))) ((Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000000000000")))) ("\100921Tz") ("\SOa\985278)\SOH") (Binding) & teamIconKey .~ (Nothing))
testObject_Team_team_18 :: Team
testObject_Team_team_18 = (newTeam ((Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000400000000")))) (">9l?d") ("") (NonBinding) & teamIconKey .~ (Just ""))
testObject_Team_team_19 :: Team
testObject_Team_team_19 = (newTeam ((Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000")))) ("\147893\EOTRK\SYN") ("K\143213") (Binding) & teamIconKey .~ (Just ""))
testObject_Team_team_20 :: Team
testObject_Team_team_20 = (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ("") ("\SUB\CANGF'") (Binding) & teamIconKey .~ (Just "\134698\CAN:8\1103801"))
