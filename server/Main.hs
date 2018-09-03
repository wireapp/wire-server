module Main where

import           Web.SCIM.Server
import           Web.SCIM.Server.Mock
import           Web.SCIM.Class.Auth (Admin (..))
import           Web.SCIM.Capabilities.MetaSchema (empty)

import           Network.Wai.Handler.Warp
import qualified STMContainers.Map as STMMap
import           Control.Monad.STM (atomically)

main :: IO ()
main = do
  auth <- STMMap.newIO
  atomically $ STMMap.insert (Admin "admin", "password") "admin" auth
  storage <- TestStorage <$> STMMap.newIO <*> STMMap.newIO <*> pure auth
  run 9000 =<< app empty (nt storage)
