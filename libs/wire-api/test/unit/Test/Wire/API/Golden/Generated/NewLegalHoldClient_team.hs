{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.NewLegalHoldClient_team where

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
testObject_NewLegalHoldClient_team_1 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_1 = NewLegalHoldClient {newLegalHoldClientPrekeys = [], newLegalHoldClientLastKey = (lastPrekey ("r\EM{\1016798cJ,\1051982VU\132927~\1032769M\34478"))}
testObject_NewLegalHoldClient_team_2 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_2 = NewLegalHoldClient {newLegalHoldClientPrekeys = [], newLegalHoldClientLastKey = (lastPrekey ("wwG&ePrA\ESC\131981I3\48042\145849"))}
testObject_NewLegalHoldClient_team_3 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_3 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "D"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "i"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}], newLegalHoldClientLastKey = (lastPrekey ("9,\188849'O|"))}
testObject_NewLegalHoldClient_team_4 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_4 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\1000699"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\DC4"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ","},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], newLegalHoldClientLastKey = (lastPrekey ("e `\120447?,"))}
testObject_NewLegalHoldClient_team_5 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_5 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "bs="},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], newLegalHoldClientLastKey = (lastPrekey ("\DC3}89\US\141131?\149298\53104\ESCO"))}
testObject_NewLegalHoldClient_team_6 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_6 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "L^"},Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "\DC3"}], newLegalHoldClientLastKey = (lastPrekey (""))}
testObject_NewLegalHoldClient_team_7 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_7 = NewLegalHoldClient {newLegalHoldClientPrekeys = [], newLegalHoldClientLastKey = (lastPrekey ("BT\25158\999576N2\f\CAN!]\US\135968M\1037165."))}
testObject_NewLegalHoldClient_team_8 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_8 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "0"}], newLegalHoldClientLastKey = (lastPrekey ("\ax{\63160a\DC2GC"))}
testObject_NewLegalHoldClient_team_9 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_9 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "U"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], newLegalHoldClientLastKey = (lastPrekey (""))}
testObject_NewLegalHoldClient_team_10 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_10 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 4}, prekeyKey = "P\1076906%jr"}], newLegalHoldClientLastKey = (lastPrekey (""))}
testObject_NewLegalHoldClient_team_11 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_11 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\12591"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\987736"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\ETB"}], newLegalHoldClientLastKey = (lastPrekey ("7\GSJZ\1070076M\DLE3\ESC"))}
testObject_NewLegalHoldClient_team_12 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_12 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "m\r"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "e"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "_\1077324"}], newLegalHoldClientLastKey = (lastPrekey ("\1073960_\1106456"))}
testObject_NewLegalHoldClient_team_13 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_13 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 7}, prekeyKey = "M\994517=U)"}], newLegalHoldClientLastKey = (lastPrekey (""))}
testObject_NewLegalHoldClient_team_14 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_14 = NewLegalHoldClient {newLegalHoldClientPrekeys = [], newLegalHoldClientLastKey = (lastPrekey ("P\93817!\1053591\1106775!d\n\999058\NULn"))}
testObject_NewLegalHoldClient_team_15 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_15 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 7}, prekeyKey = "R\ETX"}], newLegalHoldClientLastKey = (lastPrekey ("6I=\1112495G\1053923\1030349\1035269\147340u\37032\1036656l"))}
testObject_NewLegalHoldClient_team_16 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_16 = NewLegalHoldClient {newLegalHoldClientPrekeys = [], newLegalHoldClientLastKey = (lastPrekey ("\ESC\1029363\DC3\1049265"))}
testObject_NewLegalHoldClient_team_17 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_17 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\62769u"}], newLegalHoldClientLastKey = (lastPrekey ("\GS/\1059217)\153409\&5^6X\1091752\GS\120288"))}
testObject_NewLegalHoldClient_team_18 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_18 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\33562"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "N"}], newLegalHoldClientLastKey = (lastPrekey ("3\55190\148958Jq\DC3~"))}
testObject_NewLegalHoldClient_team_19 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_19 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\990754"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\989795"}], newLegalHoldClientLastKey = (lastPrekey ("\USB3\1109335/[o\DEL"))}
testObject_NewLegalHoldClient_team_20 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_20 = NewLegalHoldClient {newLegalHoldClientPrekeys = [], newLegalHoldClientLastKey = (lastPrekey ("\SI(,\1110446\n"))}
