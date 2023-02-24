import asyncio
from contextlib import contextmanager
import queue
import json
import websockets
import threading
import time
from .. import api
from ..conversions import *

DEFAULT_TIMEOUT = 5

class WS:
    """
    Websocket connection abstraction.

    This object contains a number of different connections to cannon and allows
    test code to assert on events received on those websockets.
    """
    def __init__(self, msgs):
        self.msgs = msgs

    def match(self, p, user=None, timeout=DEFAULT_TIMEOUT):
        """
        Extract an event for a given user from the stream of events in a
        websocket. The event is identified by a predicate.
        """
        t0 = time.time()
        while time.time() - t0 < timeout:
            uid, e = self.msgs.get()

            # if the queue contains an exception, just raise it here
            if isinstance(e, Exception): raise e

            if p(e) and (user is None or obj_qid(user) == uid):
                return e
            self.msgs.put((uid, e))
        assert False, "event timeout expired"

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
            for user, ws in zip(users, wss):
                asyncio.create_task(get_messages(obj_qid(user), ws))
            await control.get()
            for ws in wss:
                await ws.close()
        finally:
            for cm in cms:
                await cm.__aexit__(None, None, None)

    async def get_messages(uid, ws):
        try:
            async for msg in ws:
                e = json.loads(msg, object_hook=frozendict)
                assert len(e['payload']) == 1

                # flatten event: move payload fields into top-level dict
                event = dict(e)
                for k, v in e['payload'][0].items():
                    event[k] = v
                del event['payload']

                msgs.put((uid, frozendict(event)))
        except Exception as e:
            msgs.put((uid, e))

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
