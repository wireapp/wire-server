

# Proteus to MLS Migration Guide

## Introduction

Wire is transitioning from the Proteus encryption protocol to the newer MLS (Messaging Layer Security) protocol.

This document serves as a guide for backend administrators to manage the migration from Proteus to MLS.

## Migration Overview

### Key Concepts

- **MLS**: The protocol replacing Proteus, standardized as RFC 9420. See https://www.rfc-editor.org/rfc/rfc9420.html
- **Proteus**: The encryption protocol used by Wire since 2016. It is a variation of the DoubleRatchet family of protocols first introduced by Signal. See https://github.com/wireapp/proteus
- **KeyPackages**: Provide the keying material needed for clients to participate in MLS-encrypted conversations. See https://www.rfc-editor.org/rfc/rfc9420.html#name-key-packages . Each KeyPackage includes a unique public encryption key (init_key) for securing initial messages. Unlike Proteus prekeys, only a single KeyPackage is needed for each client added to a conversation, but the same client requires a unique KeyPackage for each conversation to which they are added. Each KeyPackage in Wire's implementation is only used once.
- **Proteus to MLS Migration per-team feature configuration**: Controls the initiation and behavior of the migration process by configuring specific features. As with many Team Management settings, these settings can be hidden and set backend-wide.

### Migration Behavior and Process

The transition from Proteus to MLS involves both client-side and server-side mechanisms to ensure a smooth change to the MLS protocol.

#### Client Setup and Registration

- **MLS-Ready Clients**: Clients with API version 5 or higher are capable of MLS. These clients will register their public identity keys associated with MLS and upload their KeyPackages to the local backend, which allows them to participate in MLS-encrypted conversations.
- **Proteus group-conversations**: Conversations which only support Proteus and have not started the process of migration.
- **Mixed protocol group-conversations**: When migration is enabled, for each conversation, clients will check if a "mixed" conversation already exists, create it if it doesn't, or join it if it does. Once all participants in a "mixed" conversation have joined, the conversation is ready for "final" transition to MLS. "Mixed" conversations still use the Proteus protocol to send messages. The MLS group in a mixed conversation contains all the MLS-capable clients who are users in the conversation.
- **MLS group-conversations**: Conversations which were either created as MLS conversations or which have completed (finalized) the migration process to MLS. All messages are sent using MLS. 
- **Active clients**: Active Wire clients are those which have been online and active within the previous 4 weeks. 
- **One on one conversation migration**: Because they only have two participants, there is no need for the "mixed" conversation type for one-on-one conversations. They are migrated as soon as all active clients of both users support MLS.
- **Force migration time**: After a set time, the migration will be forced, regardless of whether all clients have joined the "mixed" conversation. This time is configurable via the `finaliseRegardlessAfter` feature config.

#### Migration Initiation and Ongoing Monitoring

- **Periodic Checks for Migration**: Clients will routinely check (at application startup and every 24 hours) for conversations that are ready to transition to the MLS protocol. This check initiates the client’s participation in the migration process.
- **Client List Updates**: Each client continuously updates and maintains the list other clients of the same user. The client list is crucial during the migration. It ensures that all participating clients are accounted for and properly transitioned to the new protocol.

#### Conditions for migration to begin.

1. MLS is enabled in the backend (the client checks by calling the `/mls/public-keys` endpoint and checks for the `removal` key. If MLS is not enabled, the client gets a code 400 `mls-not-enabled` error)
2. Client supports api v5 or higher, and therefore MLS.
3. Client support for the MLS feature in enabled (for web, that means `FEATURE_ENABLE_MLS` set to `true`)
4. MLS protocol supported by user's team (`mls` is included in "mls" feature config’s `supportedProtocols` list)
5. Proteus to MLS migration is enabled for the client's team, and the migration time for the team (`mlsMigration`.`config`.`startTime`) has passed.

#### Handling Edge Cases and Late Proteus Messages

- **Managing Late Proteus Messages**: In scenarios involving federated environments, there may be instances where late Proteus messages are received. The system is designed to handle these occurrences to maintain the continuity and integrity of conversations, ensuring that all messages, even those delayed, are correctly processed and integrated into the ongoing communication.
- **Seamless Transition**: Despite the potential for late messages, the migration process is structured to ensure a seamless transition from Proteus to MLS, with minimal disruption to ongoing conversations and communication security.

#### TODO: Include flow diagram, get from https://wearezeta.atlassian.net/wiki/spaces/ENGINEERIN/pages/746488003/Proteus+to+MLS+Migration 

## Configuration and Management

### Feature Configuration

#### MLS Feature Config

The following `mls` feature configuration items can be set at the team-level:

* `allowedCipherSuites` - configure a list of allowed cipher suites,
* `defaultCipherSuite` - configure a default cipher suite to choose when creating a new MLS group,
* `defaultProtocol` - configure a default protocol to use when creating a new MLS group,
* `protocolToggleUsers` - define a list of users who are allowed to use non-default protocol,
* `supportedProtocols` - define a list of communication protocols supported by your team 

```{note}
The numerical values for the cipher suites (`1` in the example below) are defined in the list of indices at https://www.rfc-editor.org/rfc/rfc9420.html#table-6 .
```

- Example configuration:

```yaml
mls:
  config:
    allowedCipherSuites:
      - 1
    defaultCipherSuite: 1
    defaultProtocol: "proteus"
    protocolToggleUsers:
      - "99db9768-04e3-4b5d-9268-831b6a25c4ab"
    supportedProtocols:
      - "proteus"
      - "mls"
  lockStatus: "locked"
  status: "enabled"
  ttl: "unlimited"
```

```{note}
This is configured like many other team feature configs:
It can be configured on the server in the configuration file and if that feature is unlocked it can be also overwritten on a team level by team admins.
For more details see {ref}`the team feature settings documentation<team-feature-settings>`.
```

#### MLS Migration Feature Config

For MLS migration there is a new, separate feature config field named `mlsMigration` (parallel to the "mls" field). 

"mls" feature config will remain unchanged. 

MLS migration feature configuration includes:

1. Migration initialisation field:

`startTime` - migration start date string (in ISO 8601 format). Once this time arrives, clients will initialise the migration process (no migration-related action will take place before that time). If the migration feature is enabled, but startTime value is not set (or is set to null), migration is never started.

2. Migration finalisation field:

`finaliseRegardlessAfter` - a date string (ISO 8601 format). When that time arrives, the clients will finalise migration (change the protocol field changes to "mls"), regardless if some clients or users have not joined the MLS groups of their conversations. If this value is not set, migration is never forced.

- Example configuration:

```yaml
mlsMigration:
  config:
    startTime: "2024-03-18T10:30:00.000Z"
    finaliseRegardlessAfter: "2024-05-20T10:30:00.000Z"
  lockStatus: "locked"
  status: "enabled"
  ttl: "unlimited"
```

### Team Management Panel

The Team Management Admin Panel in Wire saw significant updates, particularly concerning the user interface adjustments for managing the "mls" and "mlsMigration" feature configurations. 

These changes are designed to enhance the control and visibility for team administrators, especially regarding the new MLS protocol implementation.

#### Adjustments to "mls" Feature Configuration Interface

1. **Handling of "mls" Feature When 'Locked'**:
   - When the "mls" feature is enabled and set to "locked" status, its configuration remains visible within the Team Management feature customization panel. This visibility is contingent on the activation of a corresponding feature flag.
   - In this locked state, the "mls" configuration settings become read-only. Team administrators can view the current configuration settings, including cipher suite and default protocol settings, but cannot make any modifications. This read-only mode is visually indicated by the greying out and disabling of all configuration fields.
   - The locked status typically reflects settings that have been predefined at the backend level and are not subject to change at the team administration level.

2. **Behavior of "mls" Feature When 'Unlocked'**:
   - Conversely, if the "lockStatus" of the "mls" feature is set to "unlocked", the UI retains its traditional functionality.
   - In this state, team administrators retain the ability to modify and save the configuration settings for the "mls" feature. This flexibility allows for dynamic adaptation to specific team needs.

#### Introduction of "mlsMigration" Feature Configuration Panel

1. **New Panel for "mlsMigration" Feature**:
   - An addition to the Team Management UI is a new configuration panel dedicated to the "mlsMigration" feature.
   - This panel is designed to allow team administrators to set up or update the settings pertinent to the MLS migration process.

2. **Modifiability and Lock Status of "mlsMigration" Feature**:
   - The modifiability of the "mlsMigration" feature configuration follows the same principles as other features in the Team Management panel.
   - If the "mlsMigration" feature is "locked", the configuration becomes read-only, mirroring the behavior outlined for the "mls" feature under similar circumstances. In this state, team administrators can view but not alter the settings.
   - The "unlocked" status of the "mlsMigration" feature permits the administrators to make changes to the migration settings, offering a level of customization and adaptability.

3. **Environment Variable for Feature Visibility**:
   - To manage the visibility of the "mlsMigration" feature within the Team Management UI, a new environment variable, `FEATURE_ENABLE_MLS_MIGRATION`, is introduced.
   - This environment variable allows for the "mlsMigration" configuration to be either displayed or hidden within the team management settings, giving backend administrators control over the feature's accessibility to team managers.

