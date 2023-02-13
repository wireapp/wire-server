#!/usr/bin/env python3

from . import api
from .conversions import obj_qid, obj_id
from .response import Response
import requests


class Context:
    def __init__(self, domain, service_map, version):
        self.domain = domain
        self.service_map = service_map
        self.version = version

    def mkurl(self, service, path, *, internal=False, protocol="http"):
        port = self.service_map[service]
        if not path or path[0] != "/":
            path = "/" + path
        if self.version is not None:
            vpath = f"/v{self.version}"
        else:
            vpath = ""
        return f"{protocol}://localhost:{port}{vpath}{path}"

    @staticmethod
    def headers(user, conn_id="0"):
        return {"Z-User": obj_id(user), "Z-Connection": conn_id}

    def request(self, method, url, user=None, conn_id="0", headers=None, **kwargs):
        if headers is None:
            headers = {}
        if user is not None:
            headers = {**headers, **self.headers(user, conn_id)}
        return Response(
            method,
            url,
            kwargs,
            requests.request(method, url, headers=headers, **kwargs),
        )

    def __getattr__(self, name):
        def method(*args, **kwargs):
            return getattr(api, name)(self, *args, **kwargs)

        return method

    def versioned(self, v):
        return type(self)(self.domain, self.service_map, v)

    def check_status(self, service):
        url = self.mkurl(service, "/i/status", internal=True)
        try:
            return self.request("HEAD", url).status_code == 200
        except requests.exceptions.ConnectionError:
            return False
