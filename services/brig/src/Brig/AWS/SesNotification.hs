module Brig.AWS.SesNotification (onEvent) where

import Brig.AWS.Types
import Brig.App
import qualified Brig.Data.Blacklist as Blacklist
import Brig.Data.UserKey (userEmailKey)
import Brig.Types (Email, fromEmail)
import Imports
import System.Logger.Class (field, msg, (~~))
import qualified System.Logger.Class as Log

onEvent :: SESNotification -> AppIO ()
onEvent (MailBounce BouncePermanent es) = onPermanentBounce es
onEvent (MailBounce BounceTransient es) = onTransientBounce es
onEvent (MailBounce BounceUndetermined es) = onUndeterminedBounce es
onEvent (MailComplaint es) = onComplaint es

onPermanentBounce :: [Email] -> AppIO ()
onPermanentBounce = mapM_ $ \e -> do
  logEmailEvent "Permanent bounce" e
  Blacklist.insert (userEmailKey e)

onTransientBounce :: [Email] -> AppIO ()
onTransientBounce = mapM_ (logEmailEvent "Transient bounce")

onUndeterminedBounce :: [Email] -> AppIO ()
onUndeterminedBounce = mapM_ (logEmailEvent "Undetermined bounce")

onComplaint :: [Email] -> AppIO ()
onComplaint = mapM_ $ \e -> do
  logEmailEvent "Complaint" e
  Blacklist.insert (userEmailKey e)

logEmailEvent :: Text -> Email -> AppIO ()
logEmailEvent t e = Log.info $ field "email" (fromEmail e) ~~ msg t
