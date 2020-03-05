module Gundeck.Client
  ( unregister,
    removeUser,
  )
where

import Control.Lens (view)
import Data.Id
import Gundeck.Monad
import qualified Gundeck.Notification.Data as Notifications
import qualified Gundeck.Push.Data as Push
import Gundeck.Push.Native
import Imports

unregister :: UserId -> ClientId -> Gundeck ()
unregister uid cid = do
  toks <- filter byClient <$> Push.lookup uid Push.Quorum
  deleteTokens toks Nothing
  where
    byClient = (cid ==) . view addrClient

removeUser :: UserId -> Gundeck ()
removeUser user = do
  toks <- Push.lookup user Push.Quorum
  deleteTokens toks Nothing
  Push.erase user
  Notifications.deleteAll user
