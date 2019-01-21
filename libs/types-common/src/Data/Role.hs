{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Data.Role
    ( Role (..)
    , defaultRole
    , rolePermissions

    , Permissions
    , newPermissions
    , fullPermissions
    , noPermissions
    , serviceWhitelistPermissions
    , self
    , copy

    , Perm (..)
    , permToInt
    , permsToInt
    , intToPerm
    , intToPerms
    ) where

import Imports
import Control.Lens hiding ((#), (.=))
import Data.Aeson hiding (Value)
import Data.Bits (testBit, (.|.))
import Data.Json.Util
import Data.Set as Set
#ifdef WITH_CQL
import qualified Control.Error.Util as Err
import qualified Database.CQL.Protocol as Cql
#endif


-- types

data Permissions = Permissions
    { _self :: Set Perm
    , _copy :: Set Perm
    } deriving (Eq, Ord, Show)

data Perm =
      CreateConversation
    | DeleteConversation
    | AddTeamMember
    | RemoveTeamMember
    | AddRemoveConvMember
    | ModifyConvMetadata
    | GetBilling
    | SetBilling
    | SetTeamData
    | GetMemberPermissions
    | SetMemberPermissions
    | GetTeamConversations
    | DeleteTeam
    -- FUTUREWORK: make the verbs in the roles more consistent
    -- (CRUD vs. Add,Remove vs; Get,Set vs. Create,Delete etc).
    -- If you ever think about adding a new permission flag,
    -- read Note [team roles] first.
    deriving (Eq, Ord, Show, Enum, Bounded)

data Role = RoleOwner | RoleAdmin | RoleMember | RoleCollaborator
    deriving (Eq, Ord, Show, Enum, Bounded)

makeLenses ''Permissions


-- helpers

defaultRole :: Role
defaultRole = RoleMember

rolePermissions :: Role -> Permissions
rolePermissions role = Permissions p p  where p = rolePerms role

rolePerms :: Role -> Set Perm
rolePerms RoleOwner = rolePerms RoleAdmin <> Set.fromList
    [ GetBilling
    , SetBilling
    , DeleteTeam
    ]
rolePerms RoleAdmin = rolePerms RoleMember <> Set.fromList
    [ AddTeamMember
    , RemoveTeamMember
    , SetTeamData
    , SetMemberPermissions
    ]
rolePerms RoleMember = rolePerms RoleCollaborator <> Set.fromList
    [ DeleteConversation
    , AddRemoveConvMember
    , ModifyConvMetadata
    , GetMemberPermissions
    ]
rolePerms RoleCollaborator = Set.fromList
    [ CreateConversation
    , GetTeamConversations
    ]

newPermissions
    :: Set Perm            -- ^ User's permissions
    -> Set Perm            -- ^ Permissions that the user will be able to
                           --   grant to other users (must be a subset)
    -> Maybe Permissions
newPermissions a b
    | b `Set.isSubsetOf` a = Just (Permissions a b)
    | otherwise            = Nothing

fullPermissions :: Permissions
fullPermissions = let p = intToPerms maxBound in Permissions p p

noPermissions :: Permissions
noPermissions = Permissions mempty mempty

-- | Permissions that a user needs to be considered a "service whitelist
-- admin" (can add and remove services from the whitelist).
serviceWhitelistPermissions :: Set Perm
serviceWhitelistPermissions = Set.fromList
    [ AddTeamMember, RemoveTeamMember
    , AddRemoveConvMember
    , SetTeamData
    ]


-- bitmasks

permToInt :: Perm -> Word64
permToInt CreateConversation       = 0x0001
permToInt DeleteConversation       = 0x0002
permToInt AddTeamMember            = 0x0004
permToInt RemoveTeamMember         = 0x0008
permToInt AddRemoveConvMember      = 0x0010
permToInt ModifyConvMetadata       = 0x0020
permToInt GetBilling               = 0x0040
permToInt SetBilling               = 0x0080
permToInt SetTeamData              = 0x0100
permToInt GetMemberPermissions     = 0x0200
permToInt GetTeamConversations     = 0x0400
permToInt DeleteTeam               = 0x0800
permToInt SetMemberPermissions     = 0x1000

intToPerm :: Word64 -> Maybe Perm
intToPerm 0x0001 = Just CreateConversation
intToPerm 0x0002 = Just DeleteConversation
intToPerm 0x0004 = Just AddTeamMember
intToPerm 0x0008 = Just RemoveTeamMember
intToPerm 0x0010 = Just AddRemoveConvMember
intToPerm 0x0020 = Just ModifyConvMetadata
intToPerm 0x0040 = Just GetBilling
intToPerm 0x0080 = Just SetBilling
intToPerm 0x0100 = Just SetTeamData
intToPerm 0x0200 = Just GetMemberPermissions
intToPerm 0x0400 = Just GetTeamConversations
intToPerm 0x0800 = Just DeleteTeam
intToPerm 0x1000 = Just SetMemberPermissions
intToPerm _      = Nothing

intToPerms :: Word64 -> Set Perm
intToPerms n =
    let perms = [ 2^i | i <- [0 .. 62], n `testBit` i ] in
    Set.fromList (mapMaybe intToPerm perms)

permsToInt :: Set Perm -> Word64
permsToInt = Set.foldr' (\p n -> n .|. permToInt p) 0


-- json

instance ToJSON Permissions where
    toJSON p = object
        $ "self" .= permsToInt (_self p)
        # "copy" .= permsToInt (_copy p)
        # []

instance FromJSON Permissions where
    parseJSON = withObject "permissions" $ \o -> do
        s <- intToPerms <$> o .: "self"
        d <- intToPerms <$> o .: "copy"
        case newPermissions s d of
            Nothing -> fail "invalid permissions"
            Just ps -> pure ps

instance ToJSON Role where
    toJSON RoleOwner        = "owner"
    toJSON RoleAdmin        = "admin"
    toJSON RoleMember       = "member"
    toJSON RoleCollaborator = "collaborator"

instance FromJSON Role where
    parseJSON = withText "Role" $ \case
        "owner"        -> pure RoleOwner
        "admin"        -> pure RoleAdmin
        "member"       -> pure RoleMember
        "collaborator" -> pure RoleCollaborator
        bad            -> fail $ "not a role: " <> show bad


-- cql

#ifdef WITH_CQL
instance Cql.Cql Role where
    ctype = Cql.Tagged Cql.IntColumn

    toCql RoleOwner        = Cql.CqlInt 1
    toCql RoleAdmin        = Cql.CqlInt 2
    toCql RoleMember       = Cql.CqlInt 3
    toCql RoleCollaborator = Cql.CqlInt 4

    fromCql (Cql.CqlInt i) = case i of
        1 -> return RoleOwner
        2 -> return RoleAdmin
        3 -> return RoleMember
        4 -> return RoleCollaborator
        n -> fail $ "Unexpected Role value: " ++ show n
    fromCql _ = fail "Role value: int expected"

instance Cql.Cql Permissions where
    ctype = Cql.Tagged $ Cql.UdtColumn "permissions" [("self", Cql.BigIntColumn), ("copy", Cql.BigIntColumn)]

    toCql p =
        let f = Cql.CqlBigInt . fromIntegral . permsToInt in
        Cql.CqlUdt [("self", f (p^.self)), ("copy", f (p^.copy))]

    fromCql (Cql.CqlUdt p) = do
        let f = intToPerms . fromIntegral :: Int64 -> Set.Set Perm
        s <- Err.note "missing 'self' permissions" ("self" `lookup` p) >>= Cql.fromCql
        d <- Err.note "missing 'copy' permissions" ("copy" `lookup` p) >>= Cql.fromCql
        r <- Err.note "invalid permissions" (newPermissions (f s) (f d))
        pure r
    fromCql _ = fail "permissions: udt expected"
#endif
