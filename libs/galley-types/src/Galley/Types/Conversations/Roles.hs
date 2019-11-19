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
import Data.Attoparsec.Text
import Data.ByteString.Conversion
import Data.Bits (testBit, (.|.))
import Data.Hashable
import qualified Data.Set as Set

data ConversationRole = ConvRoleWireAdmin
                      | ConvRoleWireMember
                      | ConvRoleCustom RoleName Actions
                      deriving (Eq, Show)

instance ToJSON ConversationRole where
    toJSON cr = object
        [ "conversation_role" .= roleToRoleName cr
        , "actions"           .= (actionsToInt $ Actions (roleActions cr))
        ]

instance FromJSON ConversationRole where
    parseJSON = withObject "conversationRole" $ \o -> do
        role    <- o .: "conversation_role"
        actions <- intToActions <$> o .: "actions"
        case (toConvRole role (Just actions)) of
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

data Actions = Actions
    { allowedActions :: Set Action
    } deriving (Eq, Ord, Show, Generic)

data Action =
      AddConvMember
    | RemoveConvMember
    | ModifyConvName
    | ModifyConvMessageTimer
    | ModifyConvReceiptMode
    | ModifyConvAccess
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)

intToAction :: Word64 -> Maybe Action
intToAction 0x0001 = Just AddConvMember
intToAction 0x0002 = Just RemoveConvMember
intToAction 0x0004 = Just ModifyConvName
intToAction 0x0008 = Just ModifyConvMessageTimer
intToAction 0x0010 = Just ModifyConvReceiptMode
intToAction 0x0020 = Just ModifyConvAccess
intToAction _      = Nothing

actionToInt :: Action -> Word64
actionToInt AddConvMember            = 0x0001
actionToInt RemoveConvMember         = 0x0002
actionToInt ModifyConvName           = 0x0004
actionToInt ModifyConvMessageTimer   = 0x0008
actionToInt ModifyConvReceiptMode    = 0x0010
actionToInt ModifyConvAccess         = 0x0020

intToActions :: Word64 -> Actions
intToActions n =
    let actions = [ 2^i | i <- [0 .. 62], n `testBit` i ] in
    Actions $ Set.fromList (mapMaybe intToAction actions)

actionsToInt :: Actions -> Word64
actionsToInt (Actions as) = Set.foldr' (\p n -> n .|. actionToInt p) 0 as

allActions :: Actions
allActions = intToActions maxBound

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
roleToRoleName ConvRoleWireAdmin    = RoleName "wire_admin"
roleToRoleName ConvRoleWireMember   = RoleName "wire_member"
roleToRoleName (ConvRoleCustom l _) = l

toConvRole :: RoleName -> Maybe Actions -> Maybe ConversationRole
toConvRole (RoleName "wire_admin")  _        = Just ConvRoleWireAdmin
toConvRole (RoleName "wire_member") _        = Just ConvRoleWireMember
toConvRole x                       (Just as) = Just (ConvRoleCustom x as)
toConvRole _                        _        = Nothing

roleActions :: ConversationRole -> Set Action
roleActions ConvRoleWireAdmin  = allowedActions allActions
roleActions ConvRoleWireMember = Set.fromList
    [ AddConvMember
    , RemoveConvMember
    ]
roleActions (ConvRoleCustom _ (Actions actions)) = actions
