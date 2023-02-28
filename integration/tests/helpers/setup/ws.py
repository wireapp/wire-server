import asyncio
from contextlib import contextmanager
import queue
import json
import websockets
import random
import threading
import time
from urllib.parse import urlencode

from .. import api
from ..conversions import *

DEFAULT_TIMEOUT = 0.1

class Timeout(Exception):
    pass

def random_conn_id():
    return str(random.randrange(0, 4294967296))

class WS:
    """
    Websocket connection abstraction.

    This object contains a number of different connections to cannon and allows
    test code to assert on events received on those websockets.
    """
    def __init__(self, msgs):
        self.msgs = msgs

    def match(self, p=lambda e: True, user=None, client=None,
              type=None, timeout=DEFAULT_TIMEOUT):
        """
        Extract an event for a given user from the stream of events in a
        websocket. The event is identified by a predicate.
        """
        endtime = time.time() + timeout
        processed = []
        try:
            while True:
                remaining = endtime - time.time()
                if remaining <= 0: break
                e = self.msgs.get(timeout=remaining)

                # if the queue contains an exception, just raise it here
                if isinstance(e, Exception): raise e

                def check():
                    if not p(e): return False
                    if user is not None and obj_qid(user) != e['target_user']:
                        return False
                    if client is not None and client != e['target_client']:
                        return False
                    if type is not None and e['type'] != type:
                        return False
                    return True

                if check(): return e
                processed.append(e)
        except queue.Empty:
            pass
        finally:
            for e in processed: self.msgs.put(e)
        raise Timeout()

@contextmanager
def ws_connect_users(ctx, *users):
    keys = {}

    # preprocess users
    def user_to_userclient(u):
        if hasattr(u, 'user') and hasattr(u, 'client'):
            return (u.user, u.client)
        elif isinstance(u, tuple) and len(u) == 2:
            return u
        else:
            return (u, None)
    users = [user_to_userclient(u) for u in users]

    msgs = queue.Queue()
    control = asyncio.Queue()

    def mkurl(user, client):
        url = '/await'
        if client is not None:
            url += '?' + urlencode({'client': client})
        url = ctx.mkurl('cannon', url, protocol='ws')
        return url

    async def connect():
        cms = [websockets.connect(mkurl(user, client),
                                  extra_headers=ctx.headers(user,
                                    conn_id=random_conn_id()))
               for user, client in users]
        return cms, [await cm.__aenter__() for cm in cms]

    async def main(cm, wss):
        try:
            for (user, client), ws in zip(users, wss):
                asyncio.create_task(get_messages(obj_qid(user), client, ws))
            await control.get()
            for ws in wss:
                await ws.close()
        finally:
            for cm in cms:
                await cm.__aexit__(None, None, None)

    async def get_messages(uid, client, ws):
        try:
            async for msg in ws:
                e = json.loads(msg, object_hook=frozendict)
                assert len(e['payload']) == 1

                # flatten event: move payload fields into top-level dict
                event = dict(e)
                for k, v in e['payload'][0].items():
                    event[k] = v
                del event['payload']

                # add target
                event['target_user'] = uid
                event['target_client'] = client

                msgs.put(frozendict(event))
        except Exception as e:
            msgs.put(e)

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
