#!/usr/bin/env python3

import websockets
import asyncio
import requests
import base64 
import wire.otr_pb2 as otr
import uuid
import sys

users = {
    1: {'idx': 1, 'id': '13cfb002-6f07-434a-90fa-1422e8141a30', 'client': '139da7a7e0034030' },
    2: {'idx': 2, 'id': 'f0e07e83-b573-4689-b366-5efa4a859a72', 'client': '6D1CB8D43AFC3EBB'},
    3: {'idx': 3, 'id': '8673c02b-651d-4f4a-96d8-4dbd51fa3e1b', 'client': 'b51351d821a734a3' },
}

domains = {
    1: {'domain': 'bund-next-column-1.wire.link', 'galley_port': 6085, 'cannon_port': 6086},
    2: {'domain': 'bund-next-column-2.wire.link', 'galley_port': 7085, 'cannon_port': 7086},
    3: {'domain': 'bund-next-column-offline-web.wire.link', 'galley_port': 11085, 'cannon_port': 11086}
}

convs = [
    {
        'user_idxs': set([1, 2]),
        'conv_id': 'eabb40cc-bf99-5a50-bd56-60c120830235',
        'domain_idx': 2
    }
]


def send_msg(user_from, conv, payload=b'hello world'):
    domain_conv = domains[conv['domain_idx']]
    domain_from = domains[user_from['idx']]
    url = f'http://localhost:{domain_from["galley_port"]}/v4/conversations/{domain_conv["domain"]}/{conv["conv_id"]}/proteus/messages'
    client_identities = [{'user': users[i]['id'], 'domain': domains[i]['domain'], 'client': users[i]['client'] } for i in conv['user_idxs'] if i != user_from['idx']]
    data = mk_otr(user_from['client'], client_identities, payload)
    response = requests.post(url, headers={'content-type': 'application/x-protobuf', 'z-user': user_from['id'], 'z-connection': 'con'}, data=data)

def get_conv(user_from, user_to):
    for c in convs:
        if set([user_from['idx'], user_to['idx']]) == c['user_idxs']:
            return c
    return ValueError('could not find cov')

async def open_websocket(user_idx):
    user = users[user_idx]
    print(f'open web socket for user {user["id"]}')
    domain = domains[user['idx']]
    # TODO: urlencode
    url = f'ws://localhost:{domain["cannon_port"]}/await?client={user["client"]}'
    print(url)
    headers = {"Z-User": user["id"], "Z-Connection": "con"}
    print(headers)
    async with websockets.connect(url, extra_headers=headers, open_timeout=4 * 60) as ws:
        print('connected')
        while True:
            message = await ws.recv()
            print(message)

def main():
    user_from_idx = 2
    user_to_idx = 1

    user_from = users[user_from_idx]
    user_to = users[user_to_idx]

    conv = get_conv(user_from, user_to)
    send_msg(user_from, conv)

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

def mk_user_entry(domain, user, client):
    user_id = mk_user_id(user)
    clients = [mk_client_entry(client)]
    user_entry = otr.UserEntry(user=user_id, clients = clients )
    return otr.QualifiedUserEntry(domain=domain, entries=[user_entry])

def mk_otr(sender_client_id_hex, client_identities, payload=b'foobar'):
    sender = mk_client_id(sender_client_id_hex)

    recipients = []
    for cid in client_identities:
        recipient = mk_user_entry(**cid)
        recipients.append(recipient)

    # report_all = otr.ClientMismatchStrategy.ReportAll()
    ignore_all = otr.ClientMismatchStrategy.IgnoreAll()
    # m = otr.QualifiedNewOtrMessage(sender=sender, recipients=recipients, blob=payload, report_all=report_all)
    m = otr.QualifiedNewOtrMessage(sender=sender, recipients=recipients, blob=payload, ignore_all=ignore_all)
    return m.SerializeToString()

if __name__ == '__main__':
    if sys.argv[1] == 'send':
        main()
    elif sys.argv[1] == 'receive':
        asyncio.run(open_websocket(1))
    else:
        raise ValueError('')