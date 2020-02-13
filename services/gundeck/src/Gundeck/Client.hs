module Gundeck.Client
  ( unregister,
    removeUser,
  )
where

import Control.Lens (view)
import Data.Id
import Data.Predicate
import Gundeck.Monad
import qualified Gundeck.Notification.Data as Notifications
import qualified Gundeck.Push.Data as Push
import Gundeck.Push.Native
import Imports
import Network.Wai (Response)
import Network.Wai.Utilities

unregister :: UserId ::: ClientId -> Gundeck Response
unregister (uid ::: cid) = do
  toks <- filter byClient <$> Push.lookup uid Push.Quorum
  deleteTokens toks Nothing
  return empty
  where
    byClient = (cid ==) . view addrClient

removeUser :: UserId -> Gundeck Response
removeUser user = do
  toks <- Push.lookup user Push.Quorum
  deleteTokens toks Nothing
  Push.erase user
  Notifications.deleteAll user
  return empty
