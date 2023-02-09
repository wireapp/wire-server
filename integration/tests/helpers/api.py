import json
import random
import requests
import string

DEFAULT_PASSWORD = 's3cret'

def random_letters(n=10):
    return ''.join(random.choices(string.ascii_letters, k=n))

def random_email():
    return 'test-email' + '-' + random_letters(10) + '@example.com'

class Context:
    def __init__(self, service_map):
        self.service_map = service_map

    def mkurl(self, service, path, *, internal=False):
        port = self.service_map[service]
        if not path or path[0] != '/':
            path = '/' + path
        return f'http://localhost:{port}{path}'

    def send(self, args, additional_args):
        # TODO: merge headers
        args = dict(**args, **additional_args)
        return self.send_(**args)

    def send_(self, method, url, **kwargs):
        return Response(method, url, kwargs, 
                        requests.request(method, url, **kwargs))

class Response:
    def __init__(self, method, url, request, response):
        self.method = method
        self.url = url
        self.request = request
        self.response = response

    def __enter__(self):
        return self.response

    def __exit__(self, etype, evalue, traceback):
        if evalue is not None:
            print(f"{self.method} {self.url}:")

            # print request JSON if present
            req = self.request.get('json')
            if req is not None:
                print("request body:")
                print(json.dumps(req, indent=True))

            # print response status code and JSON if present
            print("status code:", self.response.status_code)
            try:
                resp = self.response.json()
                print("response body:")
                print(json.dumps(resp, indent=True))
            except requests.exceptions.JSONDecodeError:
                pass


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
