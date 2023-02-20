from . import api
from .response import Response
import requests

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
        return self.request(**args)

    def request(self, method, url, **kwargs):
        return Response(method, url, kwargs, 
                        requests.request(method, url, **kwargs))
    
    def __getattr__(self, name):
        def method(*args, **kwargs):
            return getattr(api, name)(self, *args, **kwargs)
        return method
