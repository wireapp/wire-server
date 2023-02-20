import json
import random
import requests
import string

DEFAULT_PASSWORD = 's3cret'

def random_letters(n=10):
    return ''.join(random.choices(string.ascii_letters, k=n))

def random_email():
    return 'test-email' + '-' + random_letters(10) + '@example.com'

def qid_path(qid):
    return qid['domain'] + '/' + qid['id']

def user_id(user):
    if isinstance(user, str):
        return user
    else:
        return user['id']

def user_path(user):
    if 'qualified_id' in user:
        user = user['qualified_id']
    return qid_path(user)

def conv_path(conv):
    if 'qualified_id' in conv:
        conv = conv['qualified_id']
    return qid_path(conv)

def std_headers(user=None):
    headers = {'Z-Connection': '0'}
    if user is not None:
        headers['Z-User'] = user_id(user)
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
        url = f'/i/clients/{user_id(user)}'
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
        'url': ctx.mkurl('brig', f'/clients/{user_id(user)}')
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
        'url': ctx.mkurl('brig', f'/users/{user_path(target)}/clients'),
        'headers': std_headers(user)
    }
    return ctx.send(args, kwargs)

def create_connection(ctx, user, target, **kwargs):
    args = {
        'method': 'POST',
        'url': ctx.mkurl('brig', f'/connections/{user_path(target)}'),
        'headers': std_headers(user)
    }
    return ctx.send(args, kwargs)

def update_connection(ctx, user, target, status, **kwargs):
    args = {
        'method': 'PUT',
        'url': ctx.mkurl('brig', f'/connections/{user_path(target)}'),
        'headers': std_headers(user),
        'json': {'status': status}
    }
    return ctx.send(args, kwargs)

def create_conversation(ctx, user, users=None, **kwargs):
    if users is None: users = []
    args = {
        'method': 'POST',
        'url': ctx.mkurl('galley', '/conversations'),
        'headers': std_headers(user),
        'json': {'qualified_users': [u['qualified_id'] for u in users]}
    }
    return ctx.send(args, kwargs)

def get_conversation(ctx, user, conv, **kwargs):
    args = {
        'method': 'GET',
        'url': ctx.mkurl('galley', f'/conversations/{conv_path(conv)}'),
        'headers': std_headers(user)
    }
    return ctx.send(args, kwargs)
