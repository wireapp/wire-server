MLSStats - Data for monitoring Proteus to MLS migration
=======================================================

MLSStats extracts the data relevant to an ongoing migration from Proteus to MLS and stores the data as four files to an S3 bucket,
- `user-client.csv`,
- `conv-group-team-protocol.csv`,
- `domain-user-client-group.csv`, and
- `user-conv.csv`.

The tool is supposed to run (not more often than) once a day, preferably from a (Kubernetes) cron job.

## Important note

This cron job is _not_ meant for general use! It can leak data about one team to other teams.

## How to interpret the data

There are two tables with generic data from both protocols, `user-client.cvs` and `user-conv.cvs`, and two tables with MLS-specific data, `conv-group-team-protocol.csv` and `domain-user-client-group.csv`. In order to draw conclusions about the progress of the migration, the generic data has to be related to MLS-specific data.


### Use-case: conversation state ratio

The protocol used in a conversation can be Proteus, Mixed, and MLS. Mixed conversations support Proteus clients as well as MLS clients. All team conversations and their currently supported protocols can be found in `conv-group-team-protocol.csv`.

An example counting protocols per team and in total is implemented in the function `team_conversations()` in `analysis/mlsstats.py`.

### Use-case: Proteus vs MLS client ratio

In MLS, each conversation is additionally represented by a _group_.
- The mapping from group to conversation can be derived from `conv-group-team-protocol.csv`.
- The MLS clients for each user in each conversation (via the group-to-conversation mapping) can be counted in `domain-user-client-group.csv`. The domain in this table can be ignored.
- The total number of clients for each user can be counted in `user-client.csv`. The number of Proteus clients per user is the difference between total number of clients and MLS clients for this user.
- With the Proteus and MLS clients for each user sorted out, `user-conv.csv` can be used to derive the Proteus and MLS clients for each conversation by summing up the clients for each user in the conversation.

This is implemented in the function `conversation_clients()` in `analysis/mlsstats.py`.

## How to locally run MLSStats

MLSStats accepts a number of arguments for configuring the connection to Cassandra and S3. With the local environment set up, the only argument required is `--s3-bucket-name`.

## How to run MLSStats in the cluster

MLSStats displays all its command line arguments when called with the `--help` flag. Close to all arguments have to be provided in order to make the tool correctly conntect to the database and S3.
