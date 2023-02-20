from . import api
from .response import Response
import requests

class Context:
    def __init__(self, service_map, version):
        self.service_map = service_map
        self.version = version

    def mkurl(self, service, path, *, internal=False, protocol='http'):
        port = self.service_map[service]
        if not path or path[0] != '/':
            path = '/' + path
        if self.version is not None:
            vpath = f'/v{self.version}'
        else:
            vpath = ''
        return f'{protocol}://localhost:{port}{vpath}{path}'

    def send(self, args, additional_args):
        # TODO: merge headers
        args = dict(**args, **additional_args)
        return self.request(**args)

    def request(self, method, url, **kwargs):
        return Response(method, url, kwargs, 
                        requests.request(method, url, **kwargs))
    
    def __getattr__(self, name):
        def method(*args, **kwargs):
            return getattr(api, name)(self, *args, **kwargs)
        return method

    def versioned(self, v):
        return type(self)(self.service_map, v)
