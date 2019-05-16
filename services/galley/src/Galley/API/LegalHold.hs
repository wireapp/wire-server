module Galley.API.LegalHold where

import Imports
-- import Cassandra (result, hasMore)
-- import Control.Lens hiding (from, to)
-- import Control.Monad.Catch
-- import Data.ByteString.Conversion hiding (fromList)
import Data.Id
-- import Data.List1 (list1)
-- import Data.Range
-- import Data.Time.Clock (getCurrentTime, UTCTime (..))
-- import Data.Set (fromList)
import Galley.App
-- import Galley.API.Error
import Galley.API.Util
-- import Galley.Data.Types
-- import Galley.Data.Services (BotMember)
-- import Galley.Intra.Push
-- import Galley.Intra.User
-- import Galley.Options
-- import Galley.Types.Teams
-- import Galley.Types.Teams.Intra
-- import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (setStatus, result, or)
import Network.Wai.Utilities
-- import UnliftIO (mapConcurrently)
import Brig.Types.Team.LegalHold

-- import qualified Data.Set as Set
-- import qualified Galley.Data as Data
-- import qualified Galley.External as External
-- import qualified Galley.Queue as Q
-- import qualified Galley.Types as Conv
-- import qualified Galley.Types.Teams as Teams
-- import qualified Galley.Intra.Journal as Journal
-- import qualified Galley.Intra.Spar as Spar

createSettings :: UserId ::: TeamId ::: JsonRequest NewLegalHoldService ::: JSON -> Galley Response
createSettings = undefined
