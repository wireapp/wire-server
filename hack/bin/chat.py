#!/usr/bin/env python3
#
# With this tool you can send and receive (unencrypted) messages in conversations.
# It exists to test basic message sending and monitoring of events without relying on using a client.
#
# Create a config file (see example.yaml).
#
# 1. generate a shell script that sets up the port-forwarding for domain col1 via kubectl
# chat.py --config example.yaml port-forward --domain col1
#
# 2. open websockets and listen on the clients for user u1 and u2
# chat.py --config example.yaml listen --user u1 --user u2
# 
# 3. send a message to conv 1+2 with as user u1
# the message will be unencrypted plaintext, so normal clients won't be able to display it
# chat.py --config example.yaml send --user u1 --conv 1+2
#
# # example.yaml
# 
# users:
#   # pick any short name for user name
#   u1:
#     id: 13cfb002-6f07-434a-90fa-1422e8141a30
#     domain_idx: col1
#     client: 139da7a7e0034030
#   u2:
#     id: f0e07e83-b573-4689-b366-5efa4a859a72
#     domain_idx: col2
#     client: 1BD4B2DCE638BD9E
#     comment: User en7ump0q@wire.com
#   off:
#     id: 8673c02b-651d-4f4a-96d8-4dbd51fa3e1b
#     client: b51351d821a734a3
#     domain_idx: offline-web
# convs:
#   # pick any short name for conversation names
#   1+2:
#     id: eabb40cc-bf99-5a50-bd56-60c120830235
#     domain_idx: col2
# domains:
#   # pick any short name for the domain
#   col1:
#     domain: bund-next-column-1.wire.link
#     cannon_port: 6086
#     galley_port: 6085
#     namespace: wire
#   col2:
#     domain: bund-next-column-2.wire.link
#     cannon_port: 7086
#     galley_port: 7085
#     namespace: wire
#   offline-web:
#     domain: bund-next-column-offline-web.wire.link
#     cannon_port: 11086
#     galley_port: 11085
#     namespace: column-offline-web

import websockets
import asyncio
import argparse
import requests
import base64 
import wire.otr_pb2 as otr
import uuid
import sys
import subprocess
import random
import json
import datetime
import yaml
import itertools
import tempfile


port_forward_script = '''
#!/usr/bin/env bash

set -eo pipefail
domain="{domain}"
namespace="{namespace}"
galley_port="{galley_port}"
cannon_port="{cannon_port}"

actual_domain=$(kubectl -n wire get configmap brig -o yaml | sed -n 's/.*setFederationDomain: \(.*\)/\\1/p')
if [ ! "$actual_domain" = "$domain" ]; then echo "Error: backend is $actual_domain, but expected $domain"  ; exit 1; fi

set -x
kubectl -n wire port-forward $(kubectl -n wire get pods -lapp=galley -o=custom-columns=name:.metadata.name --no-headers) $galley_port:8080 &
pid1="$!"
kubectl -n wire port-forward $(kubectl -n wire get pods -lapp=cannon -o=custom-columns=name:.metadata.name --no-headers) $cannon_port:8080 &
pid2="$!"
set +x

sleep 1
read -n 1 -p "Press ENTER to kill port-forwarding processes $pid1 and $pid2:";
kill "$pid1"
kill "$pid2"
'''

def random_string():
    hiragana = [ "a", "i", "u", "e", "o", "ka", "ki", "ku", "ke", "ko", "sa", "shi", "su", "se", "so",\
                 "ta", "chi", "tsu", "te", "to", "na", "ni", "nu", "ne", "no", "ha", "hi", "fu", "he",\
                 "ho", "ma", "mi", "mu", "me", "mo", "ya", "yu", "yo", "ra", "ri", "ru", "re", "ro", "wa", "wo" ]
    s = ''
    n = random.choice([2,3,4])
    words = []
    for _ in range(n):
        l = random.choice([2,3])
        word = ''
        for i in range(l):
            word += random.choice(hiragana)
        words.append(word)
    return '_'.join(words)

def get_human_time():
    t = datetime.datetime.now()
    return t.strftime('%H:%M:%S')

def client_identities_from_missing(missing):
    cids = []
    for domain, users in missing.items():
        for user_id, client_ids in users.items():
            for client_id in client_ids:
                cids.append({'user': user_id, 'domain': domain, 'client': client_id})
    return cids

class App:
    def __init__(self, cfg):
        self.cfg = cfg
        for k, v in self.cfg['users'].items():
            v['idx'] = k
        for k, v in self.cfg['domains'].items():
            v['idx'] = k
        for k, v in self.cfg['convs'].items():
            v['idx'] = k

    def user(self, idx):
        return self.cfg['users'][idx]

    def domain(self, idx):
        return self.cfg['domains'][idx]

    def conv(self, name):
        return self.cfg['convs'][name]

    def user_idx(self, user_id):
        for k, v in self.cfg['users'].items():
            if v['id'] == user_id:
                return v
        return f'No user found for <{user_id}>'

    def conv_idx(self, conv_id):
        for k, v in self.cfg['convs'].items():
            if v['id'] == conv_id:
                return v
        return f'No conv found for <{conv_id}>'

    def send_msg(self, user_from_idx, conv_name):
        msg = get_human_time() + ' ' + random_string()
        payload = msg.encode('utf8')
        conv = self.conv(conv_name)
        user_from = self.user(user_from_idx)
        domain_conv = self.domain(conv['domain_idx'])
        domain_from = self.domain(user_from['domain_idx'])
        url = f'http://localhost:{domain_from["galley_port"]}/v4/conversations/{domain_conv["domain"]}/{conv["id"]}/proteus/messages'

        # client_identities = []
        # data = mk_otr(user_from['client'], client_identities, payload)

        data = mk_otr(user_from['client'], [], payload)
        response = requests.post(url, headers={'content-type': 'application/x-protobuf', 'z-user': user_from['id'], 'z-connection': 'con'}, data=data)
        if response.status_code != 412:
            print('got not 412')
            print(response.status_code, response.text)
            print(':(')
            sys.exit(1)
        b = response.json()

        client_identities = client_identities_from_missing(b['missing'])
        data = mk_otr(user_from['client'], client_identities, payload)

        response = requests.post(url, headers={'content-type': 'application/x-protobuf', 'z-user': user_from['id'], 'z-connection': 'con'}, data=data)
        if response.status_code != 201:
            print('got not 201')
            print(response.status_code, response.text)
            print(':(')
            sys.exit(1)

        else:
            print(f'{user_from_idx} sent: {msg}')

    async def open_websocket(self, user_idx):
        user = self.cfg['users'][user_idx]
        # print(f'open web socket for user {user["id"]}')
        domain = self.cfg['domains'][user['domain_idx']]
        # TODO: urlencode
        url = f'ws://127.0.0.1:{domain["cannon_port"]}/await?client={user["client"]}'
        # print(url)
        headers = {"Z-User": user["id"], "Z-Connection": random_string()}
        # print(headers)
        async with websockets.connect(url, extra_headers=headers, open_timeout=4 * 60) as ws:
            print(f'{user_idx} opened a websocket')
            while True:
                message_raw = await ws.recv()
                n = json.loads(message_raw.decode('utf8'))
                payload = n['payload'][0]
                type_ = payload['type']
                if type_ == 'conversation.otr-message-add':

                    conv = self.conv_idx(payload['conversation'])
                    sender_user_id = payload['qualified_from']['id']
                    sender = self.user_idx(sender_user_id)
                    msg = base64.b64decode(payload['data']['data']).decode('utf8')
                    print(f'{get_human_time()} {user_idx} receives in conv {conv["idx"]} from {sender["idx"]}: {msg}')
                else:
                    print(f'{get_human_time()} {user_idx} receives event fo type {type_}')

    async def open_websockets(self, users):
        await asyncio.gather(*[self.open_websocket(u) for u in users])

    def print_port_forward(self, domain_idx):
        d = self.domain(domain_idx)
        domain = d['domain']
        namespace = d['namespace']
        galley_port = d['galley_port']
        cannon_port = d['cannon_port']
        script = port_forward_script.format(domain=domain, namespace=namespace, galley_port=galley_port, cannon_port=cannon_port)
        with tempfile.NamedTemporaryFile(prefix=f'{domain}-port-forward-', suffix='.sh', delete=False, mode='w') as f:
            print(f'Wrote port-forward script to {f.name}')
            f.write(script)

async def main_test_websocket():
    await open_websocket(3)

def client_id_to_int(client_id):
    return int("0x" + client_id, 16)

def hex_to_bytes(hex):
    return bytes(bytearray.fromhex(hex))

def uuid_to_bytes(uuid_string):
    u = uuid.UUID(uuid_string)
    return u.bytes

def mk_client_id(client_hex):
    return otr.ClientId(client=client_id_to_int(client_hex))

def mk_client_entry(client_hex):
    client_id = mk_client_id(client_hex)
    return otr.ClientEntry(client=client_id, text=hex_to_bytes(client_hex))

def mk_user_id(uuid_string):
    uuid_bytes = uuid_to_bytes(uuid_string)
    return otr.UserId(uuid=uuid_bytes)

def mk_user_entry(user, clients):
    user_id = mk_user_id(user)
    clients = [mk_client_entry(c) for c in clients]
    return otr.UserEntry(user=user_id, clients = clients )

def mk_qualified_user_entry(domain, users):
    entries = [mk_user_entry(u, users[u]) for u in users]
    return otr.QualifiedUserEntry(domain=domain, entries=entries)

def mk_otr(sender_client_id_hex, client_identities, payload=b'foobar'):
    sender = mk_client_id(sender_client_id_hex)

# mk_qualified_user_entry

    gdomain = lambda c: c['domain']
    guser = lambda c: c['user']
    recipients = []
    for domain, users_flat in itertools.groupby(sorted(client_identities, key=gdomain), key=gdomain):
        users = {}
        for user_id, clients_flat in itertools.groupby(sorted(users_flat, key=guser), key=guser):
            users[user_id] = [c['client'] for c in clients_flat]
        recipients.append(mk_qualified_user_entry(domain, users))

    report_all = otr.ClientMismatchStrategy.ReportAll()
    # ignore_all = otr.ClientMismatchStrategy.IgnoreAll()
    m = otr.QualifiedNewOtrMessage(sender=sender, recipients=recipients, blob=payload, report_all=report_all)
    # m = otr.QualifiedNewOtrMessage(sender=sender, recipients=recipients, blob=payload, ignore_all=ignore_all)
    return m.SerializeToString()

def main_port_forward(cfg, domain):
    app = App(cfg)
    app.print_port_forward(domain)

def main_send(cfg, user, conv):
    app = App(cfg)
    app.send_msg(user, conv)

def main_listen(cfg, users):
    app = App(cfg)
    asyncio.run(app.open_websockets(users))

def main():
    parser = argparse.ArgumentParser(
        prog=sys.argv[0], description="Send and receive proteus messages across backends"
    )


    subparsers = parser.add_subparsers(
        title="subcommand", description="valid subcommands", dest="subparser_name"
    )

    parser.add_argument("--config", type=str, required=True)

    sp = subparsers.add_parser("send")
    sp.add_argument("--user", type=str, required=True)
    sp.add_argument("--conv", type=str, required=True)

    lp = subparsers.add_parser("listen")
    lp.add_argument('--user', action='append', help='can be provided multiple times')

    pf = subparsers.add_parser("port-forward")
    pf.add_argument("--domain", type=str, required=True)

    args = parser.parse_args()

    with open(args.config, 'r') as f:
        cfg = yaml.safe_load(f)

    if args.subparser_name == "send":
        main_send(cfg, args.user, args.conv)

    elif args.subparser_name == "port-forward":
        main_port_forward(cfg, args.domain)

    elif args.subparser_name == "listen":
        main_listen(cfg, args.user)


if __name__ == '__main__':
    main()
