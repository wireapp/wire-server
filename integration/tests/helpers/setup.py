from . import api
from .conversions import *

import asyncio
from contextlib import contextmanager
import itertools
import json
import queue
import time
import threading
import websockets

DEFAULT_TIMEOUT = 5

"""Higher level utilities. Useful for setting up tests without carefully
checking that the endpoints used are working correctly."""

def random_user(ctx, **kwargs):
    with ctx.create_user(**kwargs) as r:
        assert r.status_code == 201
        return r.json()

def create_client(ctx, uid, prekeys):
    with ctx.add_client(uid, [prekeys.pop()], prekeys.lpop(), ctype='permanent') as r:
        assert r.status_code == 201
        return r.json()['id']

def connected_users(ctx, num):
    users = [random_user(ctx) for _ in range(num)]

    for u1, u2 in itertools.combinations(users, 2):
        ctx.create_connection(u1, u2)
        ctx.update_connection(u2, u1, 'accepted')

    return users

class WS:
    def __init__(self, msgs):
        self.msgs = msgs

    def match(self, p, user=None, timeout=DEFAULT_TIMEOUT):
        t0 = time.time()
        while time.time() - t0 < timeout:
            e = self.next_event()
            if p(e): return e
            self.msgs.put(e)
        assert False, "event timeout expired"

    def next_event(self):
        e = json.loads(self.msgs.get())
        assert len(e['payload']) == 1

        # flatten event: move payload fields into top-level dict
        for k, v in e['payload'][0].items():
            e[k] = v
        del e['payload']

        e['qualified_conversation'] = QID.from_obj(e['qualified_conversation'])
        return e

@contextmanager
def ws_connect_users(ctx, *users):
    keys = {}

    msgs = queue.Queue()
    control = asyncio.Queue()

    url = ctx.mkurl('cannon', '/await', protocol='ws')

    async def connect():
        cms = [websockets.connect(url, extra_headers=api.std_headers(user))
               for user in users]
        return cms, [await cm.__aenter__() for cm in cms]

    async def main(cm, wss):
        try:
            for ws in wss:
                asyncio.create_task(get_messages(ws))
            await control.get()
            for ws in wss:
                await ws.close()
        finally:
            for cm in cms:
                await cm.__aexit__(None, None, None)

    async def get_messages(ws):
        async for msg in ws:
            msgs.put(msg)

    async def shutdown():
        await control.put(None)

    loop = asyncio.new_event_loop()
    cms, wss = loop.run_until_complete(connect())
    t = threading.Thread(target=lambda: loop.run_until_complete(main(cms, wss)))
    try:
        t.start()
        yield WS(msgs)
    finally:
        asyncio.run_coroutine_threadsafe(shutdown(), loop)
        t.join()
