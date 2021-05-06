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
testObject_NewLegalHoldClient_team_1 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\DC4"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "#"}], newLegalHoldClientLastKey = (lastPrekey ("d\SOw%B`X"))}
testObject_NewLegalHoldClient_team_2 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_2 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\1066585"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "h3"}], newLegalHoldClientLastKey = (lastPrekey ("\1079012 \152767'HAw\DC2zn\61421\60461jAQ"))}
testObject_NewLegalHoldClient_team_3 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_3 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\1073149"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\1103861"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}], newLegalHoldClientLastKey = (lastPrekey ("W"))}
testObject_NewLegalHoldClient_team_4 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_4 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "B"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\GS"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "M"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\1023136"}], newLegalHoldClientLastKey = (lastPrekey ("\NUL\DLE"))}
testObject_NewLegalHoldClient_team_5 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_5 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\t"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\35064"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "!"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "*"}], newLegalHoldClientLastKey = (lastPrekey ("\ETX0ed MZ"))}
testObject_NewLegalHoldClient_team_6 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_6 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}], newLegalHoldClientLastKey = (lastPrekey ("3\1093765\GS\v}u\EOT\NAK'\SUB'\1096463"))}
testObject_NewLegalHoldClient_team_7 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_7 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "Q"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "O"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\SUB"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], newLegalHoldClientLastKey = (lastPrekey (":6"))}
testObject_NewLegalHoldClient_team_8 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_8 = NewLegalHoldClient {newLegalHoldClientPrekeys = [], newLegalHoldClientLastKey = (lastPrekey ("PJ\EOTEqR7m"))}
testObject_NewLegalHoldClient_team_9 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_9 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], newLegalHoldClientLastKey = (lastPrekey ("56\vS"))}
testObject_NewLegalHoldClient_team_10 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_10 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\1059055\DC10\b6\SYN>"}], newLegalHoldClientLastKey = (lastPrekey (""))}
testObject_NewLegalHoldClient_team_11 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_11 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "G"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\ETB"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "I"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "$"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], newLegalHoldClientLastKey = (lastPrekey ("W\aCi&m"))}
testObject_NewLegalHoldClient_team_12 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_12 = NewLegalHoldClient {newLegalHoldClientPrekeys = [], newLegalHoldClientLastKey = (lastPrekey ("ykkk$Z"))}
testObject_NewLegalHoldClient_team_13 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_13 = NewLegalHoldClient {newLegalHoldClientPrekeys = [], newLegalHoldClientLastKey = (lastPrekey ("\a?\1075431\\>\a\177444\63147B"))}
testObject_NewLegalHoldClient_team_14 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_14 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], newLegalHoldClientLastKey = (lastPrekey ("V#ye\988092\NAK\1000461,.\DC3(N=oU"))}
testObject_NewLegalHoldClient_team_15 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_15 = NewLegalHoldClient {newLegalHoldClientPrekeys = [], newLegalHoldClientLastKey = (lastPrekey ("\20916T"))}
testObject_NewLegalHoldClient_team_16 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_16 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\68358"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "]"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\STX"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], newLegalHoldClientLastKey = (lastPrekey ("X\1055166\140314~\RS\156079\1098764\b|\149264D"))}
testObject_NewLegalHoldClient_team_17 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_17 = NewLegalHoldClient {newLegalHoldClientPrekeys = [], newLegalHoldClientLastKey = (lastPrekey (".\1016966a"))}
testObject_NewLegalHoldClient_team_18 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_18 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], newLegalHoldClientLastKey = (lastPrekey ("uw\1050723\&1\52165\27311\992676'#"))}
testObject_NewLegalHoldClient_team_19 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_19 = NewLegalHoldClient {newLegalHoldClientPrekeys = [], newLegalHoldClientLastKey = (lastPrekey ("\a-"))}
testObject_NewLegalHoldClient_team_20 :: NewLegalHoldClient
testObject_NewLegalHoldClient_team_20 = NewLegalHoldClient {newLegalHoldClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "q"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\1079137"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\DLE"}], newLegalHoldClientLastKey = (lastPrekey ("LS"))}
