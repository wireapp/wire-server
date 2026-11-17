# Team collaborators

A team collaborator is a user (typically an external app) that is not a member
of a team, but that the team has invited to work with it.  Team admins manage
collaborators with the `/teams/{tid}/collaborators` endpoints.

## Permissions

A collaborator has a set of permissions in the team:

- `create_team_conversation`: the collaborator may create conversations in the
  team.
- `implicit_connection`: the collaborator may start conversations with members
  of the team without being connected to them.

Permissions only say what the collaborator may *do*.  They do not affect which
conversations the collaborator is a member of: changing them, including
removing all of them, does not remove the collaborator from any conversation.
A team member can always reach a collaborator of their own team, whatever the
collaborator's permissions are.

To take away a collaborator's access to the team's conversations, remove the
collaborator.

## Removing a collaborator

When a collaborator is removed from a team:

- they are removed from all conversations of the team;
- Proteus 1:1 conversations of the team with them are deleted;
- MLS 1:1 conversations with members of the team are kept, but the collaborator
  is removed from them, unless the two users can still reach each other in
  another way: an accepted connection, a shared team, or another collaboration.
  The team member keeps the conversation and its history, and the backend sends
  an MLS remove proposal for the collaborator's clients.

If the collaborator is added to the team again, the team member gets the same
MLS 1:1 conversation back, and can add the collaborator to it again.

The same applies to MLS 1:1 conversations with the team's members and
collaborators when a user is removed from the team.  When a user account is
deleted, it is removed from all its MLS 1:1 conversations the same way.
