{-# LANGUAGE OverloadedStrings #-}

module Galley.Types.Teams.Swagger where

import Data.Swagger.Build.Api
import Imports hiding (max, min)

teamsModels :: [Model]
teamsModels =
  [ team,
    teamList,
    teamMember,
    teamMemberList,
    teamConversation,
    teamConversationList,
    newBindingTeam,
    newNonBindingTeam,
    newTeamMember,
    permissions,
    event,
    memberEvent,
    convEvent,
    updateEvent,
    member,
    conversation,
    update,
    teamMemberDelete,
    teamDelete
  ]

team :: Model
team = defineModel "Team" $ do
  description "Team information"
  property "id" bytes' $
    description "team ID"
  property "creator" bytes' $
    description "team creator's user ID"
  property "name" string' $
    description "team name"
  property "icon" string' $
    description "team icon (asset ID)"
  property "icon_key" string' $ do
    description "team icon asset key"
    optional
  property "binding" bool' $
    description "user binding team"

newBindingTeam :: Model
newBindingTeam = defineModel "NewBindingTeam" $ do
  description "Required data when creating new teams"
  property "name" string' $
    description "team name"
  property "icon" string' $
    description "team icon (asset ID)"
  property "icon_key" string' $ do
    description "team icon asset key"
    optional

newNonBindingTeam :: Model
newNonBindingTeam = defineModel "newNonBindingTeam" $ do
  description "Required data when creating new regular teams"
  property "name" string' $
    description "team name"
  property "icon" string' $
    description "team icon (asset ID)"
  property "icon_key" string' $ do
    description "team icon asset key"
    optional
  property "members" (unique $ array (ref teamMember)) $ do
    description "initial team member ids (between 1 and 127)"
    optional

teamList :: Model
teamList = defineModel "TeamList" $ do
  description "list of teams"
  property "teams" (unique $ array (ref team)) $
    description "the array of teams"
  property "has_more" bool' $
    description "if more teams are available"

newTeamMember :: Model
newTeamMember = defineModel "NewTeamMember" $ do
  description "Required data when creating new team members"
  property "member" (ref teamMember) $
    description "the team member to add"

teamMember :: Model
teamMember = defineModel "TeamMember" $ do
  description "team member data"
  property "user" bytes' $
    description "user ID"
  property "permissions" (ref permissions) $ do
    description
      "The permissions this user has in the given team \
      \ (only visible with permission `GetMemberPermissions`)."
    optional -- not optional in the type, but in the json instance.  (in
    -- servant, we could probably just add a helper type for this.)
    -- TODO: even without servant, it would be nicer to introduce
    -- a type with optional permissions.
  property "created_at" dateTime' $ do
    description "Timestamp of invitation creation.  Requires created_by."
    optional
  property "created_by" bytes' $ do
    description "ID of the inviting user.  Requires created_at."
    optional
  property "legalhold_status" legalHoldStatusType $ do
    description "The state of Legal Hold compliance for the member"
    optional

legalHoldStatusType :: DataType
legalHoldStatusType =
  string $
    enum
      [ "enabled",
        "pending",
        "disabled"
      ]

permissions :: Model
permissions = defineModel "Permissions" $ do
  description
    "Permissions constrain possible member actions.\
    \ The currently defined permissions can be found in: \
    \ https://github.com/wireapp/wire-server/blob/develop/libs/galley-types/src/Galley/Types/Teams.hs#L247"
  property "self" (int64 $ min 0 . max 0x7FFFFFFFFFFFFFFF) $
    description "The permissions bitmask which applies to this user"
  property "copy" (int64 $ min 0 . max 0x7FFFFFFFFFFFFFFF) $
    description "The permissions bitmask which this user can assign to others"

teamMemberList :: Model
teamMemberList = defineModel "TeamMemberList" $ do
  description "list of team member"
  property "members" (unique $ array (ref teamMember)) $
    description "the array of team members"
  property "hasMore" bool' $
    description "true if 'members' doesn't contain all team members"

teamConversation :: Model
teamConversation = defineModel "TeamConversation" $ do
  description "team conversation data"
  property "conversation" bytes' $
    description "conversation ID"
  property "managed" bool' $
    description "Indicates if this is a managed team conversation."

teamConversationList :: Model
teamConversationList = defineModel "TeamConversationListList" $ do
  description "list of team conversations"
  property "conversations" (unique $ array (ref teamConversation)) $
    description "the array of team conversations"

event :: Model
event = defineModel "TeamEvent" $ do
  description "team event data"
  property "type" eventType $
    description "event type"
  property "team" bytes' $
    description "team ID"
  property "time" dateTime' $
    description "date and time this event occurred"
  children
    "type"
    [ memberEvent,
      convEvent,
      updateEvent
    ]

eventType :: DataType
eventType =
  string $
    enum
      [ "team.create",
        "team.delete",
        "team.update",
        "team.member-join",
        "team.member-leave",
        "team.conversation-create",
        "team.conversation-delete"
      ]

memberEvent :: Model
memberEvent = defineModel "TeamMemberEvent" $ do
  description "team member event"
  property "data" (ref member) $ description "member data"

convEvent :: Model
convEvent = defineModel "TeamConversationEvent" $ do
  description "team conversation event"
  property "data" (ref conversation) $ description "conversation data"

updateEvent :: Model
updateEvent = defineModel "TeamUpdateEvent" $ do
  description "team update event"
  property "data" (ref update) $ description "update data"

member :: Model
member =
  defineModel "MemberData"
    $ property "user" bytes'
    $ description "user ID"

conversation :: Model
conversation =
  defineModel "ConversationData"
    $ property "conv" bytes'
    $ description "conversation ID"

update :: Model
update = defineModel "TeamUpdateData" $ do
  description "team update data"
  property "name" string' $ do
    description "new team name"
    optional
  property "icon" string' $ do
    description "new icon asset id"
    optional
  property "icon_key" string' $ do
    description "new icon asset key"
    optional

teamDelete :: Model
teamDelete = defineModel "teamDeleteData" $ do
  description "Data for a team deletion request in case of binding teams."
  property "password" string' $
    description "The account password to authorise the deletion."

teamMemberDelete :: Model
teamMemberDelete = defineModel "teamDeleteData" $ do
  description "Data for a team member deletion request in case of binding teams."
  property "password" string' $
    description "The account password to authorise the deletion."
