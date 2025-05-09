module Wire.MockInterpreters.NotificationSubsystem where

import Imports
import Polysemy
import Polysemy.State
import Wire.NotificationSubsystem

inMemoryNotificationSubsystemInterpreter ::
  (Member (State [Push]) r) =>
  InterpreterFor NotificationSubsystem r
inMemoryNotificationSubsystemInterpreter = interpret \case
  PushNotifications ps -> modify (ps <>) $> ()
  PushNotificationsSlowly {} -> error "PushNotificationsSlowly: Implement on demand"
  PushNotificationAsync {} -> error "PushNotificationAsync: Implement on demand"
  CleanupUser {} -> error "CleanupUser: Implement on demand"
  UnregisterPushClient {} -> error "UnregisterPushClient: Implement on demand"
  GetPushTokens {} -> error "GetPushTokens: Implement on demand"
  SetupConsumableNotifications {} -> error "SetupConsumableNotifications: Implement on demand"
