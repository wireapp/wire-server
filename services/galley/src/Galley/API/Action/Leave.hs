module Galley.API.Action.Leave (leaveConversation) where

import Control.Lens
import Data.Id
import Data.Qualified
import Data.Time.Clock
import Galley.API.MLS.Removal
import Galley.API.Util
import Galley.Data.Conversation
import Galley.Effects
import Galley.Env (Env)
import Galley.Types.UserList
import Imports hiding ((\\))
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog
import Wire.API.Federation.Error
import Wire.NotificationSubsystem

leaveConversation ::
  ( Member TinyLog r,
    Member (Error FederationError) r,
    Member BackendNotificationQueueAccess r,
    Member ExternalAccess r,
    Member MemberStore r,
    Member NotificationSubsystem r,
    Member ProposalStore r,
    Member Random r,
    Member SubConversationStore r,
    Member (Input Env) r,
    Member (Input UTCTime) r
  ) =>
  Qualified UserId ->
  Local Conversation ->
  Sem r ()
leaveConversation origUser lconv = do
  let victims = [origUser]
  lconv' <- traverse (convDeleteMembers (toUserList lconv victims)) lconv
  -- send remove proposals in the MLS case
  traverse_ (removeUser lconv' RemoveUserIncludeMain) victims
