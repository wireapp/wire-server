from .frozendict import frozendict

import json
import requests


class Response:
    def __init__(self, method, url, request, response):
        self.method = method
        self.url = url
        self.request = request
        self.response = response

    def __enter__(self):
        return self

    def __exit__(self, etype, evalue, traceback):
        if evalue is not None:
            self.debug()

    def debug(self):
        print(f"{self.method} {self.url}:")

        # print request JSON if present
        req = self.request.get("json")
        if req is not None:
            print("request body:")
            print(json.dumps(req, indent=2))

        # print response status code and JSON if present
        print("status code:", self.response.status_code)
        print("response body:")
        try:
            resp = self.response.json()
            print(json.dumps(resp, indent=2))
        except requests.exceptions.JSONDecodeError:
            print(self.response.text)

    def check(self, prop=None, *, status=None):
        with self:
            if prop is not None:
                assert prop(self)
            if status is not None:
                assert self.status_code == status
        return self

    @property
    def status_code(self):
        return self.response.status_code

    @property
    def text(self):
        return self.response.text

    def json(self):
        with self:
            return self.response.json(object_hook=frozendict)
