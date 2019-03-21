module TestSetup where

import           Imports
import           Util.Options
import           Bilge
import           Control.Lens (makeLenses)
import qualified Cassandra    as Cql

newtype Brig    = Brig    { runBrig    :: Request -> Request }
newtype Cannon  = Cannon  { runCannon  :: Request -> Request }
newtype Gundeck = Gundeck { runGundeck :: Request -> Request }

data TestSetup = TestSetup
  { _tsManager :: Manager
  , _tsGundeck :: Gundeck
  , _tsCannon  :: Cannon
  , _tsCannon2 :: Cannon
  , _tsBrig    :: Brig
  , _tsCass    :: Cql.ClientState
  }

makeLenses ''TestSetup
