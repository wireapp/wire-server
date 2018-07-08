module Types where

import Bilge (Request)

newtype Brig    = Brig    { runBrig    :: Request -> Request }
newtype Cannon  = Cannon  { runCannon  :: Request -> Request }
newtype Gundeck = Gundeck { runGundeck :: Request -> Request }
