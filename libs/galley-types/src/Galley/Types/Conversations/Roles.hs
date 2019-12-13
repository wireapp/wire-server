{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Galley.Types.Conversations.Roles
    ( ConversationRole
    , convRoleCustom
    , convRoleWireAdmin
    , convRoleWireMember
    , wireConvRoles

    , RoleName
    , roleNameWireAdmin
    , roleNameWireMember
    , wireConvRoleNames

    , Action (..)
    , Actions (..)
    , ConversationRolesList (..)

    , isActionAllowed
    , roleNameToActions
    )
where

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
import qualified Data.Set  as Set
import qualified Data.Text as T

data Action =
      AddConversationMember
    | RemoveConversationMember
    | ModifyConversationName
    | ModifyConversationMessageTimer
    | ModifyConversationReceiptMode
    | ModifyConversationAccess
    | ModifyOtherConversationMember
    | LeaveConversation
    | DeleteConversation
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)

deriveJSON defaultOptions{ constructorTagModifier = camelTo2 '_' } ''Action

newtype Actions = Actions
    { allowedActions :: Set Action
    } deriving (Eq, Ord, Show, Generic)

-- Do not ever expose the constructors directly, custom
-- roles must be validated to obey the prefix rules
data ConversationRole = ConvRoleWireAdmin
                      | ConvRoleWireMember
                      | ConvRoleCustom RoleName Actions
                      deriving (Eq, Show)

-- Given an action and a RoleName, three possible outcomes:
-- Just True:  Yes, the action is allowed
-- Just False: No, the action is not allowed
-- Nothing:    Not enough information, this is a custom role
isActionAllowed :: Action -> RoleName -> Maybe Bool
isActionAllowed action rn
    | isCustomRoleName rn = Nothing
    | otherwise           = pure $ maybe False (action `elem`) (roleNameToActions rn)

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

wireConvRoles :: [ConversationRole]
wireConvRoles = [ ConvRoleWireAdmin
                , ConvRoleWireMember
                ]

wireConvRoleNames :: [RoleName]
wireConvRoleNames = [roleNameWireAdmin, roleNameWireMember]

roleNameWireAdmin :: RoleName
roleNameWireAdmin = RoleName "wire_admin"

roleNameWireMember :: RoleName
roleNameWireMember = RoleName "wire_member"

convRoleWireAdmin :: ConversationRole
convRoleWireAdmin = ConvRoleWireAdmin

convRoleWireMember :: ConversationRole
convRoleWireMember = ConvRoleWireMember

convRoleCustom :: RoleName -> Actions -> Maybe ConversationRole
convRoleCustom r a
    | isCustomRoleName r = Just (ConvRoleCustom r a)
    | otherwise          = Nothing

parseRoleName :: Text -> Maybe RoleName
parseRoleName t
    | isValidRoleName t = Just (RoleName t)
    | otherwise         = Nothing

-- All RoleNames should have 2-128 chars
isValidRoleName :: Text -> Bool
isValidRoleName = either (const False) (const True)
                . parseOnly customRoleName
  where
    customRoleName = count 2 (satisfy chars)
                  *> count 126 (optional (satisfy chars))
                  *> endOfInput
    chars = inClass "a-z0-9_"

--  * Custom RoleNames _must not_ start with `wire_`
isCustomRoleName :: RoleName -> Bool
isCustomRoleName (RoleName r) = isValidRoleName r && (not $ "wire_" `T.isPrefixOf` r)

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
roleNameToActions r = roleActions <$> toConvRole r Nothing

allActions :: Actions
allActions = Actions $ Set.fromList [ minBound..maxBound ]

roleActions :: ConversationRole -> Set Action
roleActions ConvRoleWireAdmin  = allowedActions allActions
roleActions ConvRoleWireMember = Set.fromList
    [ LeaveConversation
    ]
roleActions (ConvRoleCustom _ (Actions actions)) = actions
