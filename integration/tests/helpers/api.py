from base64 import b64encode
import json
import random
import requests
import string
from .conversions import *

DEFAULT_PASSWORD = 's3cret'

def random_letters(n=10):
    return ''.join(random.choices(string.ascii_letters, k=n))

def random_email():
    return 'test-email' + '-' + random_letters(10) + '@example.com'

def std_headers(user=None):
    headers = {'Z-Connection': '0'}
    if user is not None:
        headers['Z-User'] = obj_id(user)
    return headers

def create_user(ctx, email=None, password=None, name=None, **kwargs):
    if email is None:
        email = random_email()

    if password is None:
        password = DEFAULT_PASSWORD

    if name is None:
        name = email

    body = {'email': email, 'password': password, 
            'name': name, "icon": "default"}

    args = {
        'method': 'POST',
        'json': body,
        'url': ctx.mkurl('brig', '/i/users', internal=True)
    }
    return ctx.send(args, kwargs)

def add_client(ctx, user, pks, lpk, *, internal=False, ctype=None, 
               label="Test Device", model="Test Model",
               password=DEFAULT_PASSWORD, **kwargs):
    body = {
        'prekeys': pks,
        'lastkey': lpk,
        'type': ctype,
        'label': label,
        'model': model,
        'password': password
    }
    if ctype == 'legalhold':
        body['class'] = 'legalhold'
    if internal:
        url = f'/i/clients/{obj_id(user)}'
    else:
        url = '/clients'

    args = {
        'method': 'POST',
        'json': body,
        'headers': std_headers(user),
        'url': ctx.mkurl('brig', url, internal=internal)
    }
    return ctx.send(args, kwargs)

def delete_client(ctx, user, client_id, password=DEFAULT_PASSWORD, **kwargs):
    args = {
        'method': 'DELETE',
        'json': {'password': password},
        'url': ctx.mkurl('brig', f'/clients/{obj_id(user)}')
    }
    return ctx.send(args, kwargs)

def get_clients_unqualified(ctx, user, **kwargs):
    args = {
        'method': 'GET',
        'url': ctx.mkurl('brig', '/clients'),
        'headers': std_headers(user)
    }
    return ctx.send(args, kwargs)

def get_user_clients(ctx, user, target, **kwargs):
    args = {
        'method': 'GET',
        'url': ctx.mkurl('brig', f'/users/{obj_path(target)}/clients'),
        'headers': std_headers(user)
    }
    return ctx.send(args, kwargs)

def create_connection(ctx, user, target, **kwargs):
    args = {
        'method': 'POST',
        'url': ctx.mkurl('brig', f'/connections/{obj_path(target)}'),
        'headers': std_headers(user)
    }
    return ctx.send(args, kwargs)

def update_connection(ctx, user, target, status, **kwargs):
    args = {
        'method': 'PUT',
        'url': ctx.mkurl('brig', f'/connections/{obj_path(target)}'),
        'headers': std_headers(user),
        'json': {'status': status}
    }
    return ctx.send(args, kwargs)

def create_conversation(ctx, user, users=None, name=None, protocol=None, 
                        creator_client=None, **kwargs):
    data = {'qualified_users': [u['qualified_id'] for u in (users or [])]}
    if protocol is not None: data['protocol'] = protocol
    if creator_client is not None: data['creator_client'] = creator_client

    args = {
        'method': 'POST',
        'url': ctx.mkurl('galley', '/conversations'),
        'headers': std_headers(user),
        'json': data
    }
    if name is not None: args['json']['name'] = name
    return ctx.send(args, kwargs)

def get_conversation(ctx, user, conv, **kwargs):
    args = {
        'method': 'GET',
        'url': ctx.mkurl('galley', f'/conversations/{obj_path(conv)}'),
        'headers': std_headers(user)
    }
    return ctx.send(args, kwargs)

def ws_await(ctx, user, **kwargs):
    headers = std_headers(user)
    headers['Upgrade'] = 'websocket'
    headers['Connection'] = 'Upgrade'
    headers['Sec-Websocket-Key'] = 'foobar'
    args = {
        'method': 'GET',
        'url': ctx.mkurl('cannon', '/await'),
        'headers': headers
    }
    return ctx.send(args, kwargs)

def mls_message(ctx, user, message, **kwargs):
    headers = std_headers(user)
    headers['Content-Type'] = 'message/mls'
    args = {
        'method': 'POST',
        'url': ctx.mkurl('galley', '/mls/messages'),
        'headers': headers,
        'data': message
    }
    return ctx.send(args, kwargs)

def upload_key_packages(ctx, user, client, key_packages, **kwargs):
    args = {
        'method': 'POST',
        'url': ctx.mkurl('brig', f'/mls/key-packages/self/{client}'),
        'headers': std_headers(user),
        'json': {'key_packages': [b64encode(kp).decode('ascii') for kp in key_packages]}
    }
    return ctx.send(args, kwargs)

def claim_key_packages(ctx, user, target, **kwargs):
    args = {
        'method': 'POST',
        'url': ctx.mkurl('brig', f'/mls/key-packages/claim/{obj_path(target)}'),
        'headers': std_headers(user)
    }
    return ctx.send(args, kwargs)
