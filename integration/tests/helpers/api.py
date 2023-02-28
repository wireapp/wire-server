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

def create_user(ctx, email=None, password=None, name=None, **kwargs):
    if email is None:
        email = random_email()

    if password is None:
        password = DEFAULT_PASSWORD

    if name is None:
        name = email

    body = {'email': email, 'password': password, 
            'name': name, "icon": "default"}
    for k, v in kwargs: body[k] = v

    url = ctx.mkurl('brig', '/i/users', internal=True)
    return ctx.request('POST', url, json=body)

def add_client(ctx, user, pks, lpk, *, internal=False, ctype=None, 
               label="Test Device", model="Test Model",
               password=DEFAULT_PASSWORD):
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

    url = ctx.mkurl('brig', url, internal=internal)
    return ctx.request('POST', url, user=user, json=body)

def delete_client(ctx, user, client_id, password=DEFAULT_PASSWORD):
    url = ctx.mkurl('brig', f'/clients/{obj_id(user)}')
    return ctx.request('DELETE', url, user=user, json={'password': password})

def get_clients_unqualified(ctx, user, **kwargs):
    url = ctx.mkurl('brig', '/clients')
    return ctx.request('GET', url, user=user)

def get_user_clients(ctx, user, target):
    url = ctx.mkurl('brig', f'/users/{obj_path(target)}/clients')
    return ctx.request('GET', url, user=user)

def create_connection(ctx, user, target):
    url = ctx.mkurl('brig', f'/connections/{obj_path(target)}')
    return ctx.request('POST', url, user=user)

def update_connection(ctx, user, target, status):
    url = ctx.mkurl('brig', f'/connections/{obj_path(target)}')
    return ctx.request('PUT', url, user=user, json={'status': status})

def create_conversation(ctx, user, users=None, **kwargs):
    data = {'qualified_users': [obj_qid(u) for u in (users or [])],
            **kwargs}
    url = ctx.mkurl('galley', '/conversations')
    return ctx.request('POST', url, user=user, json=data)

def get_conversation(ctx, user, conv):
    url = ctx.mkurl('galley', f'/conversations/{obj_path(conv)}')
    return ctx.request('GET', url, user=user)

def mls_message(ctx, user, message):
    headers = {'Content-Type': 'message/mls'}
    url = ctx.mkurl('galley', '/mls/messages')
    return ctx.request('POST', url, user=user, data=message, headers=headers)

def mls_welcome(ctx, user, welcome):
    headers = {'Content-Type': 'message/mls'}
    url = ctx.mkurl('galley', '/mls/welcome')
    return ctx.request('POST', url, user=user, data=welcome, headers=headers)

def upload_key_packages(ctx, user, client, key_packages):
    url = ctx.mkurl('brig', f'/mls/key-packages/self/{client}')
    data = {'key_packages': [b64encode(kp).decode('ascii') for kp in key_packages]}
    return ctx.request('POST', url, user=user, json=data)

def claim_key_packages(ctx, user, target, **kwargs):
    url = ctx.mkurl('brig', f'/mls/key-packages/claim/{obj_path(target)}')
    return ctx.request('POST', url, user=user)

def remove_member(ctx, user, conv, target, **kwargs):
    url = ctx.mkurl('galley',
        f'/conversations/{obj_path(conv)}/members/{obj_path(target)}')
    return ctx.request('DELETE', url, user=user)
