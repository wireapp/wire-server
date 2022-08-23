
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


## Load tests and configuration option examples

(legacy usage; untested if this still works, provided as an example in case api-simulations will see more usage in the future)

Using a `mailboxes.json` and `users.txt` (for wire employees: check `s3://z-config/simulator/` folder for an example), in the past the following ansible script was run to run a smoke or conversation load test. File snippet provided as an example of configuration options used.

Adapt the actual run to a modern dockerized environment (i.e. without the use of ansible).

```
- name: run smoke test
  hosts: simulator
  vars:
    log_level: Info
    api_host: '{{ external_env_domain }}' # must match BRIG_COOKIE_DOMAIN
    api_port: '{{ routing_table.nginz.web.local }}'
    email_sender: # must match BRIG_EMAIL_SENDER
      prod: 'accounts@wire.com'
    internal: false
  tasks:
    # api-loadtest and api-smoketest now take a mandatory argument 'users-federation-domain'
    # introduced as a side-effect in https://github.com/wireapp/wire-server/pull/1283
    # which we parse here from brig's config
    - name: parse federationDomain from brig
      shell: "yq .config.setFederationDomain.{{ khan_env }} < roles/brig/vars/main.yml "
      register: federationDomain
      delegate_to: localhost

    - name: update /etc/hosts
      lineinfile: >
        dest=/etc/hosts
        regexp='.*{{ api_host }}$'
        line="127.0.0.1 {{ api_host }}"
        state=present
      when: internal and simulation is defined and simulation == 'smoketest'

    - shell: LOG_LEVEL={{ log_level }} LOG_BUFFER=0
             /opt/api-simulations/bin/api-smoketest
             {% if internal %}
             --api-host='{{ api_host }}'
             --api-port='{{ api_port }}'
             {% else %}
             --api-ssl
             --api-host='{{ khan_env }}-nginz-https.{{ api_host }}'
             --api-port=443
             --api-websocket-host='{{ khan_env }}-nginz-ssl.{{ api_host }}'
             --api-websocket-port=443
             {% endif %}
             --mailbox-config=/etc/api-simulations/mailboxes.json
             --users-federation-domain='{{ federationDomain.stdout }}'
             {% if email_sender[khan_env] is defined %}
             --sender-email='{{ email_sender[khan_env] }}'
             {% endif %}
             --report-dir=/tmp/simulator
             --enable-asserts
             2>&1 | ./run
      args:
        chdir: /etc/sv/simulator/log
      when: simulation is defined and simulation == 'smoketest'
  tags:
    - simulation
    - remote

- name: run conversation load test
  hosts: simulator
  vars:
    log_level: Info
    api_host: '{{ external_env_domain }}' # must match BRIG_COOKIE_DOMAIN
    api_port: '{{ routing_table.nginz.web.local }}'
    conversations_total: 1
    conversation_bots_min: 2
    conversation_bots_max: 5
    bot_messages_min: 5
    bot_messages_max: 10
    bot_assets_min: 1
    bot_assets_max: 1
    message_length_min: 1
    message_length_max: 100
    asset_size_min: 10
    asset_size_max: 1000
    assets_noinline: false
  tasks:
    # api-loadtest and api-smoketest now take a mandatory argument 'users-federation-domain'
    # introduced as a side-effect in https://github.com/wireapp/wire-server/pull/1283
    # which we parse here from brig's config
    - name: parse federationDomain from brig
      shell: "yq .config.setFederationDomain.{{ khan_env }} < roles/brig/vars/main.yml "
      register: federationDomain
      delegate_to: localhost

    - name: update /etc/hosts
      lineinfile: >
        dest=/etc/hosts
        regexp='.*{{ api_host }}$'
        line="127.0.0.1 {{ api_host }}"
        state=present
      when: simulation is defined and simulation == 'conversations'

    - shell: LOG_LEVEL={{ log_level }} LOG_BUFFER=128
             /opt/api-simulations/bin/api-loadtest
             --api-host='{{ api_host }}'
             --api-port='{{ api_port }}'
             --users-file=/etc/api-simulations/users.txt
             --users-federation-domain='{{ federationDomain.stdout }}'
             --report-dir=/tmp/simulator
             --enable-asserts
             --conversations-total {{ conversations_total }}
             --conversation-bots-min {{ conversation_bots_min }}
             --conversation-bots-max {{ conversation_bots_max }}
             --bot-messages-max {{ bot_messages_max }}
             --bot-messages-min {{ bot_messages_min }}
             --bot-assets-min {{ bot_assets_min }}
             --bot-assets-max {{ bot_assets_max }}
             --message-length-min {{ message_length_min }}
             --message-length-max {{ message_length_max }}
             --asset-size-min {{ asset_size_min }}
             --asset-size-max {{ asset_size_max }}
             {% if assets_noinline %}
             --assets-noinline
             {% endif %}
             2>&1 | ./run
      args:
        chdir: /etc/sv/simulator/log
      when: simulation is defined and simulation == 'conversations'
```
