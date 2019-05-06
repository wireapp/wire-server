# Load Testing

This package includes the `api-loadtest` executable which will run a loadtest against
a running instance of wire-server.

Example usage: 

```
api-loadtest --api-host=test.api --api-port=8080 --conversations-total=40 --conversation-passive-bots=20  --conversation-bots=30  --bot-messages=50  --report-dir=. --message-length=50 --max-events=2000  --enable-asserts --users-file=users.csv
```

See [Creating Users for Load Test](#creating-users-for-load-test) for details on creating a `users.csv` file.


## Available Options

Run `api-loadtest -h` to get the most up to date documentation.

```
  --api-host HOSTNAME      HTTP(S) API hostname or address to connect to
  --api-port PORT          HTTP(S) API port to connect to
  --api-websocket-host HOSTNAME
                           Websocket API hostname or address to connect to
  --api-websocket-port PORT
                           Websocket API port to connect to
  --api-ssl                Use TLS (HTTPS)
  --enable-asserts         Enable assertions
  --mailbox-config FILE    Path to mailbox config
  --sender-email EMAIL     Expected sender email address
                           (FROM) (default: accounts@wire.com)
  --users-file FILE        Path to users file; which is a headerless csv
                           containing a list of ALREADY EXISTING users with the columns:
                           User-Id,Email,Password
  --report-dir DIR         Output directory for reports
  --max-events NUM         Max. event inbox size per bot
  --event-timeout SECONDS  Timeout for unmatched events when assertions are
                           enabled
  --max-asserts NUM        Max. assert queue size per bot
  --assert-timeout SECONDS Timeout for assertions
  --mailbox-folder ARG     In which mailbox folder to search for emails.
                           Defaults to 'INBOX' if not specified. Can be
                           specified multiple times for multiple folders.
  --ramp-step INT          delay in microseconds between conversations start
  --ramp-total INT         time in microseconds until full load
  --conversations-total INT
                           total number of conversations
  --conversation-bots INT|INT..INT
                           number of bots in a conversation (default: (2,5))
  --conversation-passive-bots INT|INT..INT
                           number of passive bots (that don't send anything) to
                           be added along with usual bots (default: (0,0))
  --clients INT|INT..INT   number of clients per bot (default: (1,1))
  --bot-messages INT|INT..INT
                           number of text messages to post per
                           bot (default: (0,0))
  --message-length INT|INT..INT
                           length of text messages posted (default: (1,100))
  --bot-assets INT|INT..INT
                           number of assets to post per bot (default: (0,0))
  --asset-size INT|INT..INT
                           size (bytes) of assets posted (default: (10,1000))
  --step-delay INT         delay in microseconds between actions taken by a
                           single bot (default: 1000000)
  --parallel-requests INT  maximum number of parallel requests (for bots in a
                           single conversation); applies to everything except
                           sending or receiving messages (default: 10)
```

# Creating Users for Load Test

You can create test users against a given `brig` by running the following
command from the root of `wire-server`:

```bash
./deploy/services-demo/create_test_user.sh 
```

By default the script creates users on a `brig` running at `localhost:8082`;
but you may edit the script to point elsewhere if required.

E.g. to create 100 users on a brig running at `localhost:8082` and generate a valid users file:

```shell
./deploy/services-demo/create_test_user.sh -c -n 100 -h http://localhost:8082 > users.csv
```
