{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Galley.Types.Conversations.Roles where

import Imports
#ifdef WITH_CQL
import Cassandra.CQL hiding (Set)
#endif
import Control.Applicative (optional)
import Data.Aeson
import Data.Aeson.TH
import Data.Attoparsec.Text
import Data.ByteString.Conversion
import Data.Hashable
import qualified Data.Set as Set

data Action =
      AddConversationMember
    | RemoveConversationMember
    | ModifyConversationName
    | ModifyConversationMessageTimer
    | ModifyConversationReceiptMode
    | ModifyConversationAccess
    | ModifyOtherConversationMember
    | LeaveConversation
    | DeleteConvesation
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)

deriveJSON defaultOptions{ constructorTagModifier = camelTo2 '_' } ''Action

newtype Actions = Actions
    { allowedActions :: Set Action
    } deriving (Eq, Ord, Show, Generic)

data ConversationRole = ConvRoleWireAdmin
                      | ConvRoleWireMember
                      | ConvRoleCustom RoleName Actions
                      deriving (Eq, Show)

-- Three possible outcomes:
-- Just True:  Yes, the action is allowed
-- Just False: No, the action is not allowed
-- Nothing: Not enough information, this is a custom role
isActionAllowed :: Action -> RoleName -> Maybe Bool
isActionAllowed action rn
    | isCustomRoleName rn = Nothing
    | otherwise           = pure $ maybe False (action `elem`) (roleNameToActions rn)
        -- TODO: This is actually impossible, actions
        -- must always be a Just at this point

isCustomRoleName :: RoleName -> Bool
isCustomRoleName = (`notElem` wireConvRoleNames)

instance ToJSON ConversationRole where
    toJSON cr = object
        [ "conversation_role" .= roleToRoleName cr
        , "actions"           .= roleActions cr
        ]

instance FromJSON ConversationRole where
    parseJSON = withObject "conversationRole" $ \o -> do
        role    <- o .: "conversation_role"
        actions <- o .: "actions"
        case (toConvRole role (Just $ Actions actions)) of
            Just cr -> return cr
            Nothing -> fail ("Failed to parse: " ++ show o)

data ConversationRolesList = ConversationRolesList
    { convRolesList :: [ConversationRole]
    } deriving (Eq, Show)

instance ToJSON ConversationRolesList where
    toJSON (ConversationRolesList r) = object
        [ "conversation_roles" .= r
        ]

instance FromJSON ConversationRolesList where
    parseJSON = withObject "conversation-roles-list" $ \o ->
        ConversationRolesList <$> o .: "convesation_roles"

wireConvRoles :: [ConversationRole]
wireConvRoles = [ ConvRoleWireAdmin
                , ConvRoleWireMember
                ]

wireConvRoleNames :: [RoleName]
wireConvRoleNames = [roleNameWireAdmin, roleNameWireMember]

defaultConversationRoleName :: RoleName
defaultConversationRoleName = roleNameWireAdmin

roleNameWireAdmin :: RoleName
roleNameWireAdmin = RoleName "wire_admin"

roleNameWireMember :: RoleName
roleNameWireMember = RoleName "wire_member"

-- RoleNames with `wire_` prefix are reserved
-- and cannot be created by externals
newtype RoleName = RoleName { fromRoleName :: Text }
    deriving (Eq, Show, ToJSON, ToByteString, Hashable, Generic)

#ifdef WITH_CQL
deriving instance Cql RoleName
#endif

instance FromByteString RoleName where
    parser = parser >>= maybe (fail "Invalid RoleName") return . parseRoleName

instance FromJSON RoleName where
    parseJSON = withText "RoleName" $
        maybe (fail "Invalid RoleName") pure . parseRoleName

-- All RoleNames should have 2-128 chars
--  * Wire RoleNames _must_ start with `wire_`
--  * Custom RoleNames _must not_ start with `wire_`
-- TODO: Parse, don't validate!
-- TODO: Ensure the above properties
-- TODO: Should we accept other chars as `RoleName`s?
--       this will be used for search and thus be awkward to work with
parseRoleName :: Text -> Maybe RoleName
parseRoleName t
    | isValidLabel t = Just (RoleName t)
    | otherwise      = Nothing

isValidLabel :: Text -> Bool
isValidLabel = either (const False) (const True)
             . parseOnly customLabel
  where
    customLabel = count 2 (satisfy chars)
               *> count 126 (optional (satisfy chars))
               *> endOfInput
    chars = inClass "a-zA-Z0-9_-"

allActions :: Actions
allActions = Actions . Set.fromList $ [ minBound..maxBound ]

noActions :: Actions
noActions = Actions mempty

convRoleWireAdmin :: ConversationRole
convRoleWireAdmin = ConvRoleWireAdmin

convRoleWireMember :: ConversationRole
convRoleWireMember = ConvRoleWireMember

convRoleCustom :: Text -> Actions -> Maybe ConversationRole
convRoleCustom name actions = do
   roleName <- parseRoleName name
   pure $ ConvRoleCustom roleName actions

roleToRoleName :: ConversationRole -> RoleName
roleToRoleName ConvRoleWireAdmin    = roleNameWireAdmin
roleToRoleName ConvRoleWireMember   = roleNameWireMember
roleToRoleName (ConvRoleCustom l _) = l

toConvRole :: RoleName -> Maybe Actions -> Maybe ConversationRole
toConvRole (RoleName "wire_admin")  _        = Just ConvRoleWireAdmin
toConvRole (RoleName "wire_member") _        = Just ConvRoleWireMember
toConvRole x                       (Just as) = Just (ConvRoleCustom x as)
toConvRole _                        _        = Nothing

roleNameToActions :: RoleName -> Maybe (Set Action)
roleNameToActions r = do
    let convRole = toConvRole r Nothing
    roleActions <$> convRole

roleActions :: ConversationRole -> Set Action
roleActions ConvRoleWireAdmin  = allowedActions allActions
roleActions ConvRoleWireMember = Set.fromList
    [ LeaveConversation
    ]
roleActions (ConvRoleCustom _ (Actions actions)) = actions
