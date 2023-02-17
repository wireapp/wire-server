import json
import random
import requests
import string

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

    args = {
        'method': 'POST',
        'json': body,
        'url': ctx.mkurl('brig', '/i/users', internal=True)
    }
    return ctx.send(args, kwargs)
