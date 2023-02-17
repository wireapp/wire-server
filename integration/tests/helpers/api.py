import json
import random
import requests
import string

DEFAULT_PASSWORD = 's3cret'

def random_letters(n=10):
    return ''.join(random.choices(string.ascii_letters, k=n))

def random_email():
    return 'test-email' + '-' + random_letters(10) + '@example.com'

def std_headers(uid=None):
    headers = {'Z-Connection': '0'}
    if uid is not None:
        headers['Z-User'] = uid
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

def add_client(ctx, uid, pks, lpk, *, internal=False, ctype=None, 
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
        url = f'/i/clients/{uid}'
    else:
        url = '/clients'

    args = {
        'method': 'POST',
        'json': body,
        'headers': std_headers(uid),
        'url': ctx.mkurl('brig', url, internal=internal)
    }
    return ctx.send(args, kwargs)

def delete_client(ctx, uid, client_id, password=DEFAULT_PASSWORD, **kwargs):
    args = {
        'method': 'DELETE',
        'json': {'password': password},
        'url': ctx.mkurl('brig', f'/clients/{uid}')
    }
    return ctx.send(args, kwargs)

def get_clients_unqualified(ctx, uid, **kwargs):
    args = {
        'method': 'GET',
        'url': ctx.mkurl('brig', '/clients'),
        'headers': std_headers(uid)
    }
    return ctx.send(args, kwargs)

def get_user_clients(ctx, uid1, domain2, uid2, **kwargs):
    args = {
        'method': 'GET',
        'url': ctx.mkurl('brig', f'/users/{domain2}/{uid2}/clients'),
        'headers': std_headers(uid1)
    }
    return ctx.send(args, kwargs)
