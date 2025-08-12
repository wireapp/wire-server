module Wire.ConversationSubsystem.Interpreter where

import Data.Default
import Data.Id
import Data.Qualified
import Data.Range
import Data.Set qualified as Set
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.CellsState
import Wire.API.Conversation.Role
import Wire.API.MLS.Keys
import Wire.API.User
import Wire.ConversationSubsystem as Sub (ConversationSubsystem (..))
import Wire.ConversationSubsystem.Config
import Wire.ConversationSubsystem.Validation
import Wire.StoredConversation
import Wire.UserList

interpretConversationSubsystem :: InterpreterFor ConversationSubsystem r
interpretConversationSubsystem = interpret $ \case
  Sub.CreateGroupConversation lusr conn newConv -> createGroupConvImpl lusr conn newConv
  GetConversation {} -> undefined

createGroupConvImpl :: Local UserId -> Maybe ConnId -> NewConv -> Sem r CreateGroupConversation
createGroupConvImpl = undefined

-- | Return a 'NewConversation' record suitable for creating a group conversation.
newRegularConversation ::
  ( Member (Error ConversationSubsystemError) r,
    Member (Input ConversationSubsystemConfig) r
  ) =>
  Local UserId ->
  NewConv ->
  Sem r (NewConversation, ConvSizeChecked UserList UserId)
newRegularConversation lusr newConv = do
  let uncheckedUsers = newConvMembers lusr newConv
  users <- case newConvProtocol newConv of
    BaseProtocolProteusTag ->
      checkedConvSize uncheckedUsers
    BaseProtocolMLSTag -> do
      unless (null uncheckedUsers) $ throw MLSConvCreatedWithNonEmptyMemberList
      pure mempty
  let usersWithoutCreator = (,newConvUsersRole newConv) <$> fromConvSize users
      newConvUsersRoles =
        if newConv.newConvSkipCreator
          then usersWithoutCreator
          else ulAddLocal (tUnqualified lusr, roleNameWireAdmin) usersWithoutCreator
  let nc =
        NewConversation
          { metadata =
              ConversationMetadata
                { cnvmType = RegularConv,
                  cnvmCreator = Just (tUnqualified lusr),
                  cnvmAccess = access newConv,
                  cnvmAccessRoles = accessRoles newConv,
                  cnvmName = fmap fromRange (newConvName newConv),
                  cnvmMessageTimer = newConvMessageTimer newConv,
                  cnvmReceiptMode = case newConvProtocol newConv of
                    BaseProtocolProteusTag -> newConvReceiptMode newConv
                    BaseProtocolMLSTag -> Just def,
                  cnvmTeam = fmap cnvTeamId (newConvTeam newConv),
                  cnvmGroupConvType = Just newConv.newConvGroupConvType,
                  cnvmChannelAddPermission = if newConv.newConvGroupConvType == Channel then newConv.newConvChannelAddPermission <|> Just def else Nothing,
                  cnvmCellsState =
                    if newConv.newConvCells
                      then CellsPending
                      else CellsDisabled
                },
            users = newConvUsersRoles,
            protocol = newConvProtocol newConv
          }
  pure (nc, users)

------------------------------
-- MLS

isMLSEnabled :: (Member (Input ConversationSubsystemConfig) r) => Sem r Bool
isMLSEnabled = inputs (isJust . (.mlsKeys))

-- | Fail if MLS is not enabled. Only use this function at the beginning of an
-- MLS endpoint, NOT in utility functions.
assertMLSEnabled ::
  ( Member (Input ConversationSubsystemConfig) r,
    Member (Error ConversationSubsystemError) r
  ) =>
  Sem r ()
assertMLSEnabled = void getMLSPrivateKeys

getMLSPrivateKeys ::
  ( Member (Input ConversationSubsystemConfig) r,
    Member (Error ConversationSubsystemError) r
  ) =>
  Sem r (MLSKeysByPurpose MLSPrivateKeys)
getMLSPrivateKeys = note MLSNotEnabled =<< inputs (.mlsKeys)

-------------------------------
-- Other utils

newConvMembers :: Local x -> NewConv -> UserList UserId
newConvMembers loc body =
  UserList (newConvUsers body) []
    <> toUserList loc (newConvQualifiedUsers body)

accessRoles :: NewConv -> Set AccessRole
accessRoles b = fromMaybe defRole (newConvAccessRoles b)

access :: NewConv -> [Access]
access a = case Set.toList (newConvAccess a) of
  [] -> defRegularConvAccess
  (x : xs) -> x : xs
