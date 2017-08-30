{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Galley.Types.Teams
    ( Team
    , TeamBinding (..)
    , newTeam
    , teamId
    , teamCreator
    , teamName
    , teamIcon
    , teamIconKey
    , teamBinding

    , TeamList
    , newTeamList
    , teamListTeams
    , teamListHasMore

    , TeamMember
    , newTeamMember
    , userId
    , permissions
    , teamMemberJson

    , TeamMemberList
    , newTeamMemberList
    , teamMembers
    , teamMemberListJson

    , TeamConversation
    , newTeamConversation
    , conversationId
    , managedConversation

    , TeamConversationList
    , newTeamConversationList
    , teamConversations

    , Permissions
    , newPermissions
    , fullPermissions
    , hasPermission
    , self
    , copy

    , Perm (..)
    , permToInt
    , permsToInt
    , intToPerm
    , intToPerms

    , BindingNewTeam (..)
    , NonBindingNewTeam (..)
    , NewTeam
    , newNewTeam
    , newTeamName
    , newTeamIcon
    , newTeamIconKey
    , newTeamMembers

    , NewTeamMember
    , newNewTeamMember
    , ntmNewTeamMember

    , Event
    , newEvent
    , eventType
    , eventTime
    , eventTeam
    , eventData

    , EventType (..)
    , EventData (..)

    , TeamUpdateData
    , newTeamUpdateData
    , nameUpdate
    , iconUpdate
    , iconKeyUpdate

    , TeamMemberDeleteData
    , tmdAuthPassword
    , newTeamMemberDeleteData
    , TeamDeleteData
    , tdAuthPassword
    , newTeamDeleteData
    ) where

import Control.Lens (makeLenses, (^.))
import Control.Monad (when)
import Data.Aeson
import Data.Aeson.Types (Parser, Pair)
import Data.Bits (testBit, (.|.))
import Data.Id (TeamId, ConvId, UserId)
import Data.Json.Util
import Data.Maybe (mapMaybe, isNothing)
import Data.Misc (PlainTextPassword (..))
import Data.Monoid
import Data.Range
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Word

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set

data TeamBinding =
      Binding
    | NonBinding
    deriving (Eq, Show)

data Team = Team
    { _teamId      :: TeamId
    , _teamCreator :: UserId
    , _teamName    :: Text
    , _teamIcon    :: Text
    , _teamIconKey :: Maybe Text
    , _teamBinding :: TeamBinding
    } deriving (Eq, Show)

data Event = Event
    { _eventType :: EventType
    , _eventTeam :: TeamId
    , _eventTime :: UTCTime
    , _eventData :: Maybe EventData
    } deriving Eq

data EventType =
      TeamCreate
    | TeamDelete
    | TeamUpdate
    | MemberJoin
    | MemberLeave
    | MemberUpdate
    | ConvCreate
    | ConvDelete
    deriving (Eq, Show)

data EventData =
      EdTeamCreate   Team
    | EdTeamUpdate   TeamUpdateData
    | EdMemberJoin   UserId
    | EdMemberLeave  UserId
    | EdMemberUpdate UserId
    | EdConvCreate   ConvId
    | EdConvDelete   ConvId
    deriving (Eq, Show)

data TeamUpdateData = TeamUpdateData
    { _nameUpdate    :: Maybe (Range 1 256 Text)
    , _iconUpdate    :: Maybe (Range 1 256 Text)
    , _iconKeyUpdate :: Maybe (Range 1 256 Text)
    } deriving (Eq, Show)

data TeamList = TeamList
    { _teamListTeams   :: [Team]
    , _teamListHasMore :: Bool
    } deriving Show

data TeamMember = TeamMember
    { _userId      :: UserId
    , _permissions :: Permissions
    } deriving (Eq, Ord, Show)

newtype TeamMemberList = TeamMemberList
    { _teamMembers :: [TeamMember]
    }

data TeamConversation = TeamConversation
    { _conversationId      :: ConvId
    , _managedConversation :: Bool
    }

newtype TeamConversationList = TeamConversationList
    { _teamConversations :: [TeamConversation]
    }

data Permissions = Permissions
    { _self :: Set Perm
    , _copy :: Set Perm
    } deriving (Eq, Ord, Show)

data Perm =
      CreateConversation
    | DeleteConversation
    | AddTeamMember
    | RemoveTeamMember
    | AddConversationMember
    | RemoveConversationMember
    | GetBilling
    | SetBilling
    | SetTeamData
    | GetMemberPermissions
    | SetMemberPermissions
    | GetTeamConversations
    | DeleteTeam
    deriving (Eq, Ord, Show)

data NewTeam a = NewTeam
    { _newTeamName    :: Range 1 256 Text
    , _newTeamIcon    :: Range 1 256 Text
    , _newTeamIconKey :: Maybe (Range 1 256 Text)
    , _newTeamMembers :: Maybe a
    }

newtype BindingNewTeam = BindingNewTeam (NewTeam ())
newtype NonBindingNewTeam = NonBindingNewTeam (NewTeam (Range 1 127 [TeamMember]))

newtype NewTeamMember = NewTeamMember
    { _ntmNewTeamMember :: TeamMember
    }

newtype TeamMemberDeleteData = TeamMemberDeleteData
    { _tmdAuthPassword :: PlainTextPassword
    }

newtype TeamDeleteData = TeamDeleteData
    { _tdAuthPassword :: PlainTextPassword
    }

newTeam :: TeamId -> UserId -> Text -> Text -> TeamBinding -> Team
newTeam tid uid nme ico bnd = Team tid uid nme ico Nothing bnd

newTeamList :: [Team] -> Bool -> TeamList
newTeamList = TeamList

newTeamMember :: UserId -> Permissions -> TeamMember
newTeamMember = TeamMember

newTeamMemberList :: [TeamMember] -> TeamMemberList
newTeamMemberList = TeamMemberList

newTeamConversation :: ConvId -> Bool -> TeamConversation
newTeamConversation = TeamConversation

newTeamConversationList :: [TeamConversation] -> TeamConversationList
newTeamConversationList = TeamConversationList

newNewTeam :: Range 1 256 Text -> Range 1 256 Text -> NewTeam a
newNewTeam nme ico = NewTeam nme ico Nothing Nothing

newNewTeamMember :: TeamMember -> NewTeamMember
newNewTeamMember = NewTeamMember

newEvent :: EventType -> TeamId -> UTCTime -> Event
newEvent typ tid tme = Event typ tid tme Nothing

newTeamUpdateData :: TeamUpdateData
newTeamUpdateData = TeamUpdateData Nothing Nothing Nothing

newTeamMemberDeleteData :: PlainTextPassword -> TeamMemberDeleteData
newTeamMemberDeleteData = TeamMemberDeleteData

newTeamDeleteData :: PlainTextPassword -> TeamDeleteData
newTeamDeleteData = TeamDeleteData

makeLenses ''Team
makeLenses ''TeamList
makeLenses ''TeamMember
makeLenses ''TeamMemberList
makeLenses ''TeamConversation
makeLenses ''TeamConversationList
makeLenses ''Permissions
makeLenses ''NewTeam
makeLenses ''NewTeamMember
makeLenses ''Event
makeLenses ''TeamUpdateData
makeLenses ''TeamMemberDeleteData
makeLenses ''TeamDeleteData

newPermissions :: Set Perm -> Set Perm -> Maybe Permissions
newPermissions a b
    | b `Set.isSubsetOf` a = Just (Permissions a b)
    | otherwise            = Nothing

fullPermissions :: Permissions
fullPermissions = let p = intToPerms maxBound in Permissions p p

hasPermission :: TeamMember -> Perm -> Bool
hasPermission tm p = p `Set.member` (tm^.permissions.self)

permToInt :: Perm -> Word64
permToInt CreateConversation       = 0x0001
permToInt DeleteConversation       = 0x0002
permToInt AddTeamMember            = 0x0004
permToInt RemoveTeamMember         = 0x0008
permToInt AddConversationMember    = 0x0010
permToInt RemoveConversationMember = 0x0020
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
intToPerm 0x0010 = Just AddConversationMember
intToPerm 0x0020 = Just RemoveConversationMember
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

instance ToJSON TeamBinding where
    toJSON Binding    = Bool True
    toJSON NonBinding = Bool False

instance ToJSON Team where
    toJSON t = object
        $ "id"       .= _teamId t
        # "creator"  .= _teamCreator t
        # "name"     .= _teamName t
        # "icon"     .= _teamIcon t
        # "icon_key" .= _teamIconKey t
        # "binding"  .= _teamBinding t
        # []

instance FromJSON TeamBinding where
    parseJSON (Bool True)  = pure Binding
    parseJSON (Bool False) = pure NonBinding
    parseJSON other        = fail $ "Unknown binding type: " <> show other

instance FromJSON Team where
    parseJSON = withObject "team" $ \o -> do
        Team <$> o .:  "id"
             <*> o .:  "creator"
             <*> o .:  "name"
             <*> o .:  "icon"
             <*> o .:? "icon_key"
             <*> o .:? "binding" .!= NonBinding

instance ToJSON TeamList where
    toJSON t = object
        $ "teams"    .= _teamListTeams t
        # "has_more" .= _teamListHasMore t
        # []

instance FromJSON TeamList where
    parseJSON = withObject "teamlist" $ \o -> do
        TeamList <$> o .: "teams"
                 <*> o .: "has_more"

teamMemberJson :: Bool -> TeamMember -> Value
teamMemberJson False m = object [ "user" .= _userId m ]
teamMemberJson True  m = object [ "user" .= _userId m, "permissions" .= _permissions m ]

teamMemberListJson :: Bool -> TeamMemberList -> Value
teamMemberListJson withPerm l =
    object [ "members" .= map (teamMemberJson withPerm) (_teamMembers l) ]

instance FromJSON TeamMember where
    parseJSON = withObject "team-member" $ \o ->
        TeamMember <$> o .:  "user"
                   <*> o .:  "permissions"

instance FromJSON TeamMemberList where
    parseJSON = withObject "team member list" $ \o ->
        TeamMemberList <$> o .: "members"

instance ToJSON TeamConversation where
    toJSON t = object
        [ "conversation" .= _conversationId t
        , "managed"      .= _managedConversation t
        ]

instance FromJSON TeamConversation where
    parseJSON = withObject "team conversation" $ \o ->
        TeamConversation <$> o .: "conversation" <*> o .: "managed"

instance ToJSON TeamConversationList where
    toJSON t = object ["conversations" .= _teamConversations t]

instance FromJSON TeamConversationList where
    parseJSON = withObject "team conversation list" $ \o -> do
        TeamConversationList <$> o .: "conversations"

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

newTeamJson :: NewTeam a -> [Pair]
newTeamJson (NewTeam n i ik _) = 
          "name"     .= fromRange n
        # "icon"     .= fromRange i
        # "icon_key" .= (fromRange <$> ik)
        # []

instance ToJSON BindingNewTeam where
    toJSON (BindingNewTeam t) = object $ newTeamJson t

instance ToJSON NonBindingNewTeam where
    toJSON (NonBindingNewTeam t) = 
        object
        $ "members" .= (map (teamMemberJson True) . fromRange <$> _newTeamMembers t)
        # newTeamJson t

deriving instance FromJSON BindingNewTeam
deriving instance FromJSON NonBindingNewTeam

instance (FromJSON a) => FromJSON (NewTeam a) where
    parseJSON = withObject "new-team" $ \o -> do
        name <- o .:  "name"
        icon <- o .:  "icon"
        key  <- o .:? "icon_key"
        mems <- o .:? "members"
        either fail pure $ NewTeam <$> checkedEitherMsg "name" name
                                   <*> checkedEitherMsg "icon" icon
                                   <*> maybe (pure Nothing) (fmap Just . checkedEitherMsg "icon_key") key
                                   <*> pure mems

instance ToJSON NewTeamMember where
    toJSON t = object ["member" .= teamMemberJson True (_ntmNewTeamMember t)]

instance FromJSON NewTeamMember where
    parseJSON = withObject "add team member" $ \o ->
        NewTeamMember <$> o .: "member"

instance ToJSON EventType where
    toJSON TeamCreate   = String "team.create"
    toJSON TeamDelete   = String "team.delete"
    toJSON TeamUpdate   = String "team.update"
    toJSON MemberJoin   = String "team.member-join"
    toJSON MemberUpdate = String "team.member-update"
    toJSON MemberLeave  = String "team.member-leave"
    toJSON ConvCreate   = String "team.conversation-create"
    toJSON ConvDelete   = String "team.conversation-delete"

instance FromJSON EventType where
    parseJSON (String "team.create")              = pure TeamCreate
    parseJSON (String "team.delete")              = pure TeamDelete
    parseJSON (String "team.update")              = pure TeamUpdate
    parseJSON (String "team.member-join")         = pure MemberJoin
    parseJSON (String "team.member-update")       = pure MemberUpdate
    parseJSON (String "team.member-leave")        = pure MemberLeave
    parseJSON (String "team.conversation-create") = pure ConvCreate
    parseJSON (String "team.conversation-delete") = pure ConvDelete
    parseJSON other                               = fail $ "Unknown event type: " <> show other

instance ToJSON Event where
    toJSON = Object . toJSONObject

instance ToJSONObject Event where
    toJSONObject e = HashMap.fromList
        [ "type" .= _eventType e
        , "team" .= _eventTeam e
        , "time" .= _eventTime e
        , "data" .= _eventData e
        ]

instance FromJSON Event where
    parseJSON = withObject "event" $ \o -> do
        ty <- o .:  "type"
        dt <- o .:? "data"
        Event ty <$> o .: "team"
                 <*> o .: "time"
                 <*> parseEventData ty dt

instance ToJSON EventData where
    toJSON (EdTeamCreate   tem) = toJSON tem
    toJSON (EdMemberJoin   usr) = object ["user" .= usr]
    toJSON (EdMemberUpdate usr) = object ["user" .= usr]
    toJSON (EdMemberLeave  usr) = object ["user" .= usr]
    toJSON (EdConvCreate   cnv) = object ["conv" .= cnv]
    toJSON (EdConvDelete   cnv) = object ["conv" .= cnv]
    toJSON (EdTeamUpdate   upd) = toJSON upd

parseEventData :: EventType -> Maybe Value -> Parser (Maybe EventData)
parseEventData MemberJoin Nothing  = fail "missing event data for type 'team.member-join'"
parseEventData MemberJoin (Just j) = do
    let f o = Just . EdMemberJoin <$> o .: "user"
    withObject "member join data" f j

parseEventData MemberUpdate Nothing  = fail "missing event data for type 'team.member-update"
parseEventData MemberUpdate (Just j) = do
    let f o = Just . EdMemberUpdate <$> o .: "user"
    withObject "member update data" f j

parseEventData MemberLeave Nothing  = fail "missing event data for type 'team.member-leave'"
parseEventData MemberLeave (Just j) = do
    let f o = Just . EdMemberLeave <$> o .: "user"
    withObject "member leave data" f j

parseEventData ConvCreate Nothing  = fail "missing event data for type 'team.conversation-create"
parseEventData ConvCreate (Just j) = do
    let f o = Just . EdConvCreate  <$> o .: "conv"
    withObject "conversation create data" f j

parseEventData ConvDelete Nothing  = fail "missing event data for type 'team.conversation-delete"
parseEventData ConvDelete (Just j) = do
    let f o = Just . EdConvDelete  <$> o .: "conv"
    withObject "conversation delete data" f j

parseEventData TeamCreate Nothing  = fail "missing event data for type 'team.create'"
parseEventData TeamCreate (Just j) = Just . EdTeamCreate <$> parseJSON j

parseEventData TeamUpdate Nothing  = fail "missing event data for type 'team.update'"
parseEventData TeamUpdate (Just j) = Just . EdTeamUpdate <$> parseJSON j

parseEventData _ Nothing  = pure Nothing
parseEventData t (Just _) = fail $ "unexpected event data for type " <> show t

instance ToJSON TeamUpdateData where
    toJSON u = object
        $ "name"     .= _nameUpdate u
        # "icon"     .= _iconUpdate u
        # "icon_key" .= _iconKeyUpdate u
        # []

instance FromJSON TeamUpdateData where
    parseJSON = withObject "team update data" $ \o -> do
        name     <- o .:? "name"
        icon     <- o .:? "icon"
        icon_key <- o .:? "icon_key"
        when (isNothing name && isNothing icon && isNothing icon_key) $
            fail "TeamUpdateData: no update data specified"
        either fail pure $ TeamUpdateData <$> maybe (pure Nothing) (fmap Just . checkedEitherMsg "name")     name
                                          <*> maybe (pure Nothing) (fmap Just . checkedEitherMsg "icon")     icon
                                          <*> maybe (pure Nothing) (fmap Just . checkedEitherMsg "icon_key") icon_key

instance FromJSON TeamMemberDeleteData where
    parseJSON = withObject "team-member-delete-data" $ \o ->
        TeamMemberDeleteData <$> o .: "password"

instance ToJSON TeamMemberDeleteData where
    toJSON tmd = object
        [ "password" .= _tmdAuthPassword tmd
        ]

instance FromJSON TeamDeleteData where
    parseJSON = withObject "team-delete-data" $ \o ->
        TeamDeleteData <$> o .: "password"

instance ToJSON TeamDeleteData where
    toJSON tdd = object
        [ "password" .= _tdAuthPassword tdd
        ]
