
# Wire Client Simulations

Simulations for verifying the operational reliability of backend deployments
via automated black-box testing of the APIs.

**Mailbox accounts**

Simulations usually require at least one real mailbox account that is accessible
over IMAP with TLS and supports a large number of aliases of the form `foo+<anything>@bar.com`.
See `mailboxes.example.json`.

## Smoke Test

The smoke test is a simulation that is supposed to run through happy-paths
of core parts of the server APIs, covering the major use-cases but without
going into testing edge-cases, specific bugs, rare error conditions or
scalability, which is left to prior unit and integration testing as well as
dedicated load simulations.

Running the smoke test against an environment is supposed to give confidence
that it is operational and ready to accept real client connections, without
polluting it with a lot of test data, so that it can be frequently run in
production.

### Running the Smoke Test

An example of running the smoke test:

    api-smoketest
        --api-host=prod-nginz-https.wire.com
        --api-port=443
        --api-websocket-host=prod-nginz-ssl.wire.com
        --api-websocket-port=443
        --api-ssl
        --mailbox-config=./mailboxes.json
        --enable-asserts

Run `api-smoketest --help` to see all options.

The output of a typical (successful) run looks like

    2017-06-21T14:58:17Z, I, Starting Smoke Test
    2017-06-21T14:58:17Z, I, Register user: wirebot+Ally-a37203b7-875c-43f1-bfe7-1d296377980e@wire.com
    2017-06-21T14:58:17Z, I, Bot=6c7bd6df-2b24-4df6-80c1-c2077f20fafe, Tag=Ally, Await activation mail
    2017-06-21T14:58:19Z, I, Bot=6c7bd6df-2b24-4df6-80c1-c2077f20fafe, Tag=Ally, Activate user
    2017-06-21T14:58:20Z, I, Bot=6c7bd6df-2b24-4df6-80c1-c2077f20fafe, Tag=Ally, Login
    2017-06-21T14:58:20Z, I, Bot=6c7bd6df-2b24-4df6-80c1-c2077f20fafe, Tag=Ally, Establishing push channel
    2017-06-21T14:58:20Z, I, Register user: wirebot+Bill-fcd41e29-acb6-4852-becc-9b2c5592b90d@wire.com
    2017-06-21T14:58:21Z, I, Bot=1054556b-6051-4b16-985b-c39045543d16, Tag=Bill, Await activation mail
    2017-06-21T14:58:27Z, I, Bot=1054556b-6051-4b16-985b-c39045543d16, Tag=Bill, Activate user
    2017-06-21T14:58:27Z, I, Bot=1054556b-6051-4b16-985b-c39045543d16, Tag=Bill, Login
    2017-06-21T14:58:27Z, I, Bot=1054556b-6051-4b16-985b-c39045543d16, Tag=Bill, Establishing push channel
    2017-06-21T14:58:28Z, I, Register user: wirebot+Carl-ec975772-176c-4057-bc56-96b5a34a0716@wire.com
    2017-06-21T14:58:28Z, I, Bot=00dc4756-8b04-4698-8305-e86a76feccb0, Tag=Carl, Await activation mail
    2017-06-21T14:58:35Z, I, Bot=00dc4756-8b04-4698-8305-e86a76feccb0, Tag=Carl, Activate user
    2017-06-21T14:58:35Z, I, Bot=00dc4756-8b04-4698-8305-e86a76feccb0, Tag=Carl, Login
    2017-06-21T14:58:35Z, I, Bot=00dc4756-8b04-4698-8305-e86a76feccb0, Tag=Carl, Establishing push channel
    2017-06-21T14:58:35Z, I, Setting up connections
    2017-06-21T14:58:35Z, I, Bot=1054556b-6051-4b16-985b-c39045543d16, Tag=Bill, Thread=ThreadId 36, RCV: user.connection
    2017-06-21T14:58:35Z, I, Bot=1054556b-6051-4b16-985b-c39045543d16, Tag=Bill, Thread=ThreadId 40, ACK: user.connection
    2017-06-21T14:58:35Z, I, Bot=00dc4756-8b04-4698-8305-e86a76feccb0, Tag=Carl, Thread=ThreadId 49, RCV: user.connection
    2017-06-21T14:58:35Z, I, Bot=00dc4756-8b04-4698-8305-e86a76feccb0, Tag=Carl, Thread=ThreadId 53, ACK: user.connection
    2017-06-21T14:58:36Z, I, Bot=6c7bd6df-2b24-4df6-80c1-c2077f20fafe, Tag=Ally, Thread=ThreadId 23, RCV: conversation.member-join
    2017-06-21T14:58:36Z, I, Bot=6c7bd6df-2b24-4df6-80c1-c2077f20fafe, Tag=Ally, Thread=ThreadId 23, RCV: user.connection
    2017-06-21T14:58:36Z, I, Bot=6c7bd6df-2b24-4df6-80c1-c2077f20fafe, Tag=Ally, Thread=ThreadId 28, ACK: conversation.member-join
    2017-06-21T14:58:36Z, I, Bot=6c7bd6df-2b24-4df6-80c1-c2077f20fafe, Tag=Ally, Thread=ThreadId 28, ACK: user.connection
    2017-06-21T14:58:36Z, I, Bot=6c7bd6df-2b24-4df6-80c1-c2077f20fafe, Tag=Ally, Thread=ThreadId 23, RCV: conversation.member-join
    2017-06-21T14:58:36Z, I, Bot=6c7bd6df-2b24-4df6-80c1-c2077f20fafe, Tag=Ally, Thread=ThreadId 23, RCV: user.connection
    2017-06-21T14:58:36Z, I, Bot=6c7bd6df-2b24-4df6-80c1-c2077f20fafe, Tag=Ally, Thread=ThreadId 28, ACK: conversation.member-join
    2017-06-21T14:58:36Z, I, Bot=6c7bd6df-2b24-4df6-80c1-c2077f20fafe, Tag=Ally, Thread=ThreadId 28, ACK: user.connection
    2017-06-21T14:58:37Z, I, Creating conversations
    2017-06-21T14:58:37Z, I, Member state updates
    2017-06-21T14:58:37Z, I, Bot=00dc4756-8b04-4698-8305-e86a76feccb0, Tag=Carl, Thread=ThreadId 49, RCV: conversation.create
    2017-06-21T14:58:37Z, I, Bot=00dc4756-8b04-4698-8305-e86a76feccb0, Tag=Carl, Thread=ThreadId 53, ACK: conversation.create
    2017-06-21T14:58:37Z, I, Bot=1054556b-6051-4b16-985b-c39045543d16, Tag=Bill, Thread=ThreadId 36, RCV: conversation.create
    2017-06-21T14:58:37Z, I, Bot=1054556b-6051-4b16-985b-c39045543d16, Tag=Bill, Thread=ThreadId 40, ACK: conversation.create
    2017-06-21T14:58:37Z, I, Members join & leave
    2017-06-21T14:58:37Z, I, Bot=6c7bd6df-2b24-4df6-80c1-c2077f20fafe, Tag=Ally, Thread=ThreadId 23, RCV: conversation.member-leave
    2017-06-21T14:58:37Z, I, Bot=6c7bd6df-2b24-4df6-80c1-c2077f20fafe, Tag=Ally, Thread=ThreadId 28, ACK: conversation.member-leave
    2017-06-21T14:58:37Z, I, Bot=00dc4756-8b04-4698-8305-e86a76feccb0, Tag=Carl, Thread=ThreadId 49, RCV: conversation.member-leave
    2017-06-21T14:58:37Z, I, Bot=00dc4756-8b04-4698-8305-e86a76feccb0, Tag=Carl, Thread=ThreadId 53, ACK: conversation.member-leave
    2017-06-21T14:58:37Z, I, Bot=00dc4756-8b04-4698-8305-e86a76feccb0, Tag=Carl, Thread=ThreadId 49, RCV: conversation.member-join
    2017-06-21T14:58:37Z, I, Bot=6c7bd6df-2b24-4df6-80c1-c2077f20fafe, Tag=Ally, Thread=ThreadId 23, RCV: conversation.member-join
    2017-06-21T14:58:37Z, I, Bot=6c7bd6df-2b24-4df6-80c1-c2077f20fafe, Tag=Ally, Thread=ThreadId 28, ACK: conversation.member-join
    2017-06-21T14:58:37Z, I, Bot=00dc4756-8b04-4698-8305-e86a76feccb0, Tag=Carl, Thread=ThreadId 53, ACK: conversation.member-join
    2017-06-21T14:58:38Z, I, Basic search reachability
    2017-06-21T14:58:38Z, I, Registering Clients
    2017-06-21T14:58:39Z, I, OTR 1-1 greetings
    2017-06-21T14:58:40Z, I, Bot=1054556b-6051-4b16-985b-c39045543d16, Tag=Bill, Thread=ThreadId 36, RCV: conversation.otr-message-add
    2017-06-21T14:58:40Z, I, Bot=1054556b-6051-4b16-985b-c39045543d16, Tag=Bill, Thread=ThreadId 40, ACK: conversation.otr-message-add
    2017-06-21T14:58:40Z, I, Bot=00dc4756-8b04-4698-8305-e86a76feccb0, Tag=Carl, Thread=ThreadId 49, RCV: conversation.otr-message-add
    2017-06-21T14:58:40Z, I, Bot=6c7bd6df-2b24-4df6-80c1-c2077f20fafe, Tag=Ally, Thread=ThreadId 23, RCV: conversation.otr-message-add
    2017-06-21T14:58:40Z, I, Bot=00dc4756-8b04-4698-8305-e86a76feccb0, Tag=Carl, Thread=ThreadId 53, ACK: conversation.otr-message-add
    2017-06-21T14:58:40Z, I, Bot=6c7bd6df-2b24-4df6-80c1-c2077f20fafe, Tag=Ally, Thread=ThreadId 28, ACK: conversation.otr-message-add
    2017-06-21T14:58:40Z, I, Bot=6c7bd6df-2b24-4df6-80c1-c2077f20fafe, Tag=Ally, Thread=ThreadId 23, RCV: conversation.otr-message-add
    2017-06-21T14:58:40Z, I, Bot=6c7bd6df-2b24-4df6-80c1-c2077f20fafe, Tag=Ally, Thread=ThreadId 28, ACK: conversation.otr-message-add
    2017-06-21T14:58:40Z, I, Bot=1054556b-6051-4b16-985b-c39045543d16, Tag=Bill, Thread=ThreadId 36, RCV: conversation.otr-message-add
    2017-06-21T14:58:40Z, I, Bot=1054556b-6051-4b16-985b-c39045543d16, Tag=Bill, Thread=ThreadId 40, ACK: conversation.otr-message-add
    2017-06-21T14:58:40Z, I, Bot=00dc4756-8b04-4698-8305-e86a76feccb0, Tag=Carl, Thread=ThreadId 49, RCV: conversation.otr-message-add
    2017-06-21T14:58:40Z, I, Bot=00dc4756-8b04-4698-8305-e86a76feccb0, Tag=Carl, Thread=ThreadId 53, ACK: conversation.otr-message-add
    2017-06-21T14:58:40Z, I, OTR group conversation
    2017-06-21T14:58:40Z, I, Bot=1054556b-6051-4b16-985b-c39045543d16, Tag=Bill, Thread=ThreadId 36, RCV: conversation.otr-message-add
    2017-06-21T14:58:40Z, I, Bot=00dc4756-8b04-4698-8305-e86a76feccb0, Tag=Carl, Thread=ThreadId 49, RCV: conversation.otr-message-add
    2017-06-21T14:58:40Z, I, Bot=1054556b-6051-4b16-985b-c39045543d16, Tag=Bill, Thread=ThreadId 40, ACK: conversation.otr-message-add
    2017-06-21T14:58:41Z, I, Bot=6c7bd6df-2b24-4df6-80c1-c2077f20fafe, Tag=Ally, Thread=ThreadId 23, RCV: conversation.otr-message-add
    2017-06-21T14:58:41Z, I, Bot=00dc4756-8b04-4698-8305-e86a76feccb0, Tag=Carl, Thread=ThreadId 53, ACK: conversation.otr-message-add
    2017-06-21T14:58:41Z, I, Bot=00dc4756-8b04-4698-8305-e86a76feccb0, Tag=Carl, Thread=ThreadId 49, RCV: conversation.otr-message-add
    2017-06-21T14:58:41Z, I, Bot=00dc4756-8b04-4698-8305-e86a76feccb0, Tag=Carl, Thread=ThreadId 53, ACK: conversation.otr-message-add
    2017-06-21T14:58:41Z, I, Bill gets a new phone
    2017-06-21T14:58:41Z, I, Bot=6c7bd6df-2b24-4df6-80c1-c2077f20fafe, Tag=Ally, Thread=ThreadId 28, ACK: conversation.otr-message-add
    2017-06-21T14:58:42Z, I, Bot=6c7bd6df-2b24-4df6-80c1-c2077f20fafe, Tag=Ally, Thread=ThreadId 23, RCV: conversation.otr-message-add
    2017-06-21T14:58:42Z, I, Bot=6c7bd6df-2b24-4df6-80c1-c2077f20fafe, Tag=Ally, Thread=ThreadId 28, ACK: conversation.otr-message-add
    2017-06-21T14:58:42Z, I, Bot=1054556b-6051-4b16-985b-c39045543d16, Tag=Bill, Thread=ThreadId 36, RCV: conversation.otr-message-add
    2017-06-21T14:58:42Z, I, Bot=1054556b-6051-4b16-985b-c39045543d16, Tag=Bill, Thread=ThreadId 36, RCV: conversation.otr-message-add
    2017-06-21T14:58:42Z, I, Bot=1054556b-6051-4b16-985b-c39045543d16, Tag=Bill, Thread=ThreadId 40, ACK: conversation.otr-message-add
    2017-06-21T14:58:42Z, I, Bot=1054556b-6051-4b16-985b-c39045543d16, Tag=Bill, Thread=ThreadId 40, ACK: conversation.otr-message-add
    2017-06-21T14:58:42Z, I, Waiting for event & assertion timeouts (if any)
    
    Smoke Test Report
    
    2017-06-21 14:58:42.448241 UTC
    
    Bots
    	Created (New): 3
    	Created (Cached): 0
    	Alive: 0
    
    Assertions
    	Total: 88
    	Failed: 0
    
    Exceptions
    	Total: 0
    
    Events (Total)
    	Received: 25
    	Acknowledged: 25
    	Ignored: 0
    	Missed: 0
    
    Event (user.connection)
    	Received: 4
    	Acknowledged: 4
    	Ignored: 0
    	Missed: 0
    
    Event (user.new)
    	Received: 0
    	Acknowledged: 0
    	Ignored: 0
    	Missed: 0
    
    Event (conversation.create)
    	Received: 2
    	Acknowledged: 2
    	Ignored: 0
    	Missed: 0
    
    Event (conversation.member-join)
    	Received: 4
    	Acknowledged: 4
    	Ignored: 0
    	Missed: 0
    
    Event (conversation.connect-request)
    	Received: 0
    	Acknowledged: 0
    	Ignored: 0
    	Missed: 0
    
    Event (conversation.member-leave)
    	Received: 2
    	Acknowledged: 2
    	Ignored: 0
    	Missed: 0
    
    Event (conversation.rename)
    	Received: 0
    	Acknowledged: 0
    	Ignored: 0
    	Missed: 0
    
    Event (conversation.member-state-update)
    	Received: 0
    	Acknowledged: 0
    	Ignored: 0
    	Missed: 0
    
    Event (conversation.otr-message-add)
    	Received: 13
    	Acknowledged: 13
    	Ignored: 0
    	Missed: 0

