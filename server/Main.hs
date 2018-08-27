module Main where

import           Web.SCIM.Server
import           Web.SCIM.Server.Mock
import           Web.SCIM.Capabilities.MetaSchema (empty)

import           Network.Wai.Handler.Warp
import qualified STMContainers.Map as Map

main :: IO ()
main = do
  storage <- TestStorage <$> Map.newIO <*> Map.newIO
  run 9000 $ app empty (nt storage)
