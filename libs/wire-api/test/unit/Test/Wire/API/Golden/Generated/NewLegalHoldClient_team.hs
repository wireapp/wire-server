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
testObject_NewLegalHoldClient_team_1 = NewLegalHoldClient {newLegalHoldClientPrekeys = [], newLegalHoldClientLastKey = (lastPrekey ("5\DC3\DC3L?\DC3\nnQP5\FS\175642~"))}
testObject_NewLegalHoldClient_team_2 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_2 = NewLegalHoldClient {newLegalHoldClientPrekeys = [], newLegalHoldClientLastKey = (lastPrekey ("?]D+\SOH\DC3uk\1112728"))}
testObject_NewLegalHoldClient_team_3 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_3 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], newLegalHoldClientLastKey = (lastPrekey ("\165617\161739\54107\1087999\&2\1007956^,\1037834\1095199\"\SO"))}
testObject_NewLegalHoldClient_team_4 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_4 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], newLegalHoldClientLastKey = (lastPrekey ("\1076635z&E[\DC2"))}
testObject_NewLegalHoldClient_team_5 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_5 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = ""}], newLegalHoldClientLastKey = (lastPrekey ("\36509\74156\&2j"))}
testObject_NewLegalHoldClient_team_6 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_6 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], newLegalHoldClientLastKey = (lastPrekey ("_\1036022Gz@\97318w\27504\129186v+"))}
testObject_NewLegalHoldClient_team_7 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_7 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "+#a"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "j"}], newLegalHoldClientLastKey = (lastPrekey ("\NAK[Ss\ACKTG+w"))}
testObject_NewLegalHoldClient_team_8 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_8 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\1010186\f"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "!"}], newLegalHoldClientLastKey = (lastPrekey ("T\DC13v\138061\1011799}C\ACK"))}
testObject_NewLegalHoldClient_team_9 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_9 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], newLegalHoldClientLastKey = (lastPrekey ("E\ACK-\n0\1057771\t \172108:"))}
testObject_NewLegalHoldClient_team_10 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_10 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "-"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "m"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "j"}], newLegalHoldClientLastKey = (lastPrekey ("Q\1002458\b\1079973\158234L\1037864\&7"))}
testObject_NewLegalHoldClient_team_11 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_11 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\59328\6745("}], newLegalHoldClientLastKey = (lastPrekey ("i9\EM\ts9\32097\1021213\52634"))}
testObject_NewLegalHoldClient_team_12 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_12 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "*"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\1110596"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\124972"}], newLegalHoldClientLastKey = (lastPrekey ("\987525\189204\n\b\37854\177554@f\GS\ESC\1035481"))}
testObject_NewLegalHoldClient_team_13 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_13 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 4}, prekeyKey = "Tua\38423y\43640"}], newLegalHoldClientLastKey = (lastPrekey (""))}
testObject_NewLegalHoldClient_team_14 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_14 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\146318R\vn\140959"}], newLegalHoldClientLastKey = (lastPrekey ("\n#\1088899\DC3f.8\1043073\24910`b\1055417w\SUBW"))}
testObject_NewLegalHoldClient_team_15 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_15 = NewLegalHoldClient {newLegalHoldClientPrekeys = [], newLegalHoldClientLastKey = (lastPrekey (" \SUB\NAK\DC2\98070\ESC\1009637\ESCxq"))}
testObject_NewLegalHoldClient_team_16 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_16 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = ")C"},Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "i["}], newLegalHoldClientLastKey = (lastPrekey ("^a5H\1081659"))}
testObject_NewLegalHoldClient_team_17 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_17 = NewLegalHoldClient {newLegalHoldClientPrekeys = [], newLegalHoldClientLastKey = (lastPrekey ("zHK\ACK\1033571\1079800\v_a"))}
testObject_NewLegalHoldClient_team_18 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_18 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\1045192\984527"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "/"},Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "\NUL"}], newLegalHoldClientLastKey = (lastPrekey ("\1002879[j\1049100\1027958\ETX"))}
testObject_NewLegalHoldClient_team_19 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_19 = NewLegalHoldClient {newLegalHoldClientPrekeys = [], newLegalHoldClientLastKey = (lastPrekey ("O\1097126h\128335\157674\SO"))}
testObject_NewLegalHoldClient_team_20 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_20 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "(f)"},Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = ""}], newLegalHoldClientLastKey = (lastPrekey ("D\187424\DC2=\16899'\1019463Xcgx"))}
