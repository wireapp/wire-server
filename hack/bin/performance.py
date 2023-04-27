#!/usr/bin/env python3
"""
Simulate large MLS conversation for performance testing

main_setup_team() - create team and mls clients
main_setup_add_participants() - create mls conv and add team members
main_send() - send (as admin) a mls message to mls conv
main_receive() - connect to websockets and wait for message in mls conv
main_analyze() - analyze logfiles of previous functions and print analysis
"""

import re
import math
import copy
import sys
import concurrent.futures
import logging
from urllib.parse import urljoin, urlparse, urlencode, urlunparse
from contextlib import contextmanager
import random
from uuid import UUID
from http.cookies import SimpleCookie
import argparse
import asyncio
import os
import tempfile
import string
import shutil
import json
import time
from base64 import b64encode, b64decode
import websockets
from requests import Request, Session
from requests.models import Response

from wire.mlscli import (
    ClientState,
    init_mls_client,
    get_public_key,
    cid2str,
    generate_key_package,
    create_group,
    add_member,
    make_bundle,
    consume_welcome,
    create_application_message,
    consume_message,
)
from wire.conversions import obj_id

from wire import api


LAST_PREKEY = "pQABARn//wKhAFggnCcZIK1pbtlJf4wRQ44h4w7/sfSgj5oWXMQaUGYAJ/sDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g=="


def save_json_file(ob, path):
    with open(path, "w") as f:
        return json.dump(ob, f, indent=2)


def load_json_file(path):
    with open(path, "r") as f:
        return json.load(f)


def simplify_body(content):
    try:
        return json.loads(content)
    except Exception:
        try:
            if isinstance(content, bytes):
                return content.decode("utf8")
            else:
                return content
        except Exception:
            b = b64encode(content).decode("utf8")
            return {"base64data": b, "comment": "binary blob has been replaced"}


def simplify_response(response):
    """
    Returns a json-serializable structure of the response and the request.
    Request and response bodies of the input are parsed as JSON if possible.
    """

    result = {}

    result["request"] = {
        "url": response.request.url,
        "method": response.request.method,
        "body": simplify_body(response.request.body),
        "headers": dict(response.request.headers),
    }

    result["response"] = {
        "status_code": response.status_code,
        "content": simplify_body(response.content),
        "headers": dict(response.headers),
    }

    return result


re_u = re.compile("^u=([0-9a-f-]{36})$")


def get_user_of_cookie_or_token(s):
    for c in s.split("."):
        m = re.match(re_u, c)
        if m is not None:
            (uid,) = m.groups()
            return uid
    raise ValueError(f"Could not find user in cookie or token: {s}")


def pretty(ob):
    return json.dumps(ob, indent=2)


def pretty_response(response):
    d = simplify_response(response)
    return pretty(d)


class Context:
    """
    All methods in this library make calls through a Context object.
    This class exist to make dependency injection possible:
    Write your own version of Context to control of urls are build and how
    actual http request are fired.

    This Context handles re-authentication.
    """

    def __init__(self):
        self.session = Session()
        self.access_token = None

        self.users_access_tokens = {}
        self.users_zuid_cookie = {}
        self.last_switched_user = None

    def mkurl(self, service, relative_url, internal=False):
        if not internal:
            service = "nginz"

        name = "WIREAPI_BASEURL_" + service.upper()
        baseurl = os.environ[name]
        if not relative_url.startswith("/access"):
            relative_url = "/v4" + relative_url
        return urljoin(baseurl, relative_url)

    @staticmethod
    def headers(user, conn_id="0"):
        return {"Z-User": obj_id(user), "Z-Connection": conn_id}

    def request(self, method, url, user=None, conn_id="0", headers=None, **kwargs):
        if headers is None:
            headers = {}
        if user is not None:
            headers = {**headers, **self.headers(user, conn_id)}

        if headers.get("Z-User") is None and self.last_switched_user:
            headers["Z-User"] = self.last_switched_user

        req = Request(method, url, headers=headers, **kwargs)

        # dont use access token on login, because token might be from another user
        if req.url.endswith("/login"):
            self.clear_access_token()

        preq = self.session.prepare_request(req)
        if self.access_token is not None:
            preq.headers["Authorization"] = "Bearer " + self.access_token
        res = self.session.send(preq)

        # dont retry token refresh requests
        if req.url.endswith("/access"):
            self.detect_cookies(res)
            return res

        if res.status_code == 401:
            res_refresh = api.create_access_token(self)
            if res_refresh.status_code != 200:
                msg = "Refreshing the access token failed: \n"
                msg = pretty_response(res_refresh)
                raise ValueError(msg)
            else:
                d = res_refresh.json()
                self.set_access_token(d["access_token"])

                preq.headers["Authorization"] = "Bearer " + self.access_token
                res = self.session.send(preq)
                self.detect_cookies(res)
                return res
        else:
            self.detect_cookies(res)
            return res

    def load_cookies_from_response(self, ob):
        """
        This method is not used by wireapi
        """
        if isinstance(ob, dict):
            cookie_header = ob["response"]["headers"]["Set-Cookie"]
        elif isinstance(ob, Response):
            cookie_header = ob.headers["Set-Cookie"]
        else:
            raise ValueError("Could not find any cookies to load")

        cookies = SimpleCookie()
        cookies.load(cookie_header)
        self.session.cookies.update(cookies)
        self.backup_users_zuid_cookie()

    def clear_access_token(self):
        """
        This method is not used by wireapi
        """
        self.access_token = None

    def clear_zuid_cookie(self):
        """
        This method is not used by wireapi
        """
        del self.session.cookies["zuid"]

    def set_access_token(self, access_token):
        """
        This method is not used by wireapi
        """
        self.access_token = access_token
        self.backup_users_access_token()

    def detect_cookies(self, res):
        """
        Without this function the request.Session would collect
        multiple 'zuid' cookies.
        """
        set_cookie = res.headers.get("Set-Cookie")
        if set_cookie:
            # clear up potential old 'zuid' cookies
            del self.session.cookies["zuid"]

            cookies = SimpleCookie()
            cookies.load(set_cookie)
            self.session.cookies.update(cookies)
            self.backup_users_zuid_cookie()

    def backup_users_access_token(self):
        u = get_user_of_cookie_or_token(self.access_token)
        self.users_access_tokens[u] = self.access_token

    def backup_users_zuid_cookie(self):
        if len(self.session.cookies) > 0:
            zuid = self.session.cookies.get("zuid")
            if zuid is not None:
                u = get_user_of_cookie_or_token(zuid)
                self.users_zuid_cookie[u] = zuid

    def switch_user(self, user_id):
        """
        This method is not used by wireapi
        """
        self.last_switched_user = user_id

        at = self.users_access_tokens.get(user_id)
        if at is not None:
            self.access_token = at

        zuid = self.users_zuid_cookie.get(user_id)
        if zuid is not None:
            cookies = SimpleCookie()
            cookies.load(zuid)
            self.session.cookies.update(cookies)


j = os.path.join


def base64encode(payload):
    return b64encode(payload).decode("utf8")


def random_letters(n=10):
    return "".join(random.choices(string.ascii_letters, k=n))


def random_path(basedir):
    return j(j(basedir, "tmp"), random_letters())


def random_msg():
    return "i_say_" + "".join(random.choices(string.digits, k=5))


def simple_expect_status(status_code, res_simple):
    if res_simple["response"]["status_code"] != status_code:
        msg = f"Unexpect status error (expected {status_code})"
        msg += pretty(res_simple)
        raise ValueError(msg)


def upload_new_keypackage(ctx, state):
    kp, ref = generate_key_package(state)
    key_packages = [kp]
    res = api.upload_key_packages(
        ctx,
        user=None,
        client=state.client_identity["client"],
        key_packages=key_packages,
    )
    return res


def save(res, path):
    b = simplify_response(res)
    d = os.path.dirname(path)
    if not os.path.exists(d):
        os.makedirs(d)
    save_json_file(b, path)
    print(f"Saving to {path}")
    return b


def create_admin(ctx, basedir):
    ud_temp = random_path(basedir)
    os.makedirs(ud_temp, exist_ok=True)

    res_creation = save(
        api.create_user(ctx, create_team=True), j(ud_temp, "res_creation.json")
    )
    simple_expect_status(201, res_creation)

    user_id = res_creation["response"]["content"]["id"]
    ud = user_dir(basedir, user_id)
    os.makedirs(ud, exist_ok=True)

    dest = j(ud, "res_creation.json")
    shutil.move(j(ud_temp, "res_creation.json"), dest)
    print(f"Moving to {dest}")

    res_login = save(
        api.login(ctx, res_creation["request"]["body"]["email"]),
        j(ud, "res_login.json"),
    )
    simple_expect_status(200, res_login)

    meta_set(basedir, "admin_user_id", user_id)

    return user_id


def save_public_keys(ctx, basedir):
    """
    Function assumes that any user is logged in
    """
    res_mls_keys = save(api.mls_get_public_keys(ctx), j(basedir, "res_mls_keys.json"))
    simple_expect_status(200, res_mls_keys)
    removal_key = b64decode(res_mls_keys["response"]["content"]["removal"]["ed25519"])
    removal_key_file = j(basedir, "removal_key.json")
    with open(removal_key_file, "wb") as f:
        f.write(removal_key)


def post_clients(ctx):
    return api.add_client(
        ctx,
        user=None,
        pks=[],
        lpk={"id": 65535, "key": LAST_PREKEY},
        ctype="permanent",
        internal=False,
    )


def meta_get(basedir, key):
    p = j(basedir, "meta", key)
    if os.path.exists(p):
        with open(p, "r") as f:
            return json.load(f)


def meta_set(basedir, key, value):
    p = j(basedir, "meta", key)
    dir_ = os.path.dirname(p)
    if not os.path.exists(dir_):
        os.makedirs(dir_)

    with open(p, "w") as f:
        return json.dump(value, f)


def user_dir(basedir, user_id):
    return os.path.join(basedir, "users", user_id)


def admin_user_dir(basedir):
    admin_id = meta_get(basedir, "admin_user_id")
    return user_dir(basedir, admin_id)


def create_user(ctx, basedir):
    admin_dir = admin_user_dir(basedir)

    admin = load_json_file(j(admin_dir, "res_creation.json"))
    admin_user = admin["response"]["content"]["id"]
    team = admin["response"]["content"]["team"]

    admin_res_login = load_json_file(j(admin_dir, "res_login.json"))
    ctx.load_cookies_from_response(admin_res_login)

    ud_temp = random_path(basedir)
    os.makedirs(ud_temp, exist_ok=True)

    res_inv = save(api.create_team_invitation(ctx, team), j(ud_temp, "res_inv.json"))
    email_invitee = res_inv["request"]["body"]["email"]
    simple_expect_status(201, res_inv)

    invitation = res_inv["response"]["content"]["id"]
    res_invcode = save(
        api.get_invitation_code(ctx, team, invitation, user=admin_user),
        j(ud_temp, "res_invcode.json"),
    )
    simple_expect_status(200, res_invcode)
    code = res_invcode["response"]["content"]["code"]

    res_register = save(
        api.register_user(ctx, email=email_invitee, code=code),
        j(ud_temp, "res_register.json"),
    )
    simple_expect_status(201, res_register)
    assert res_register["response"]["content"]["team"] == team

    user_id = res_register["response"]["content"]["id"]
    ud = user_dir(basedir, user_id)
    os.makedirs(ud, exist_ok=True)

    dest = j(ud, "res_register.json")
    shutil.move(j(ud_temp, "res_register.json"), dest)
    print("Moving to", dest)

    res_login = save(api.login(ctx, email_invitee), j(ud, "res_login.json"))
    simple_expect_status(200, res_login)

    return user_id


def client_dir(user_dir, client_id):
    return j(user_dir, f"clients/{client_id}")


def create_mls_client(ctx, basedir, user_id):
    """
    Assummes the user is logged in
    """

    # is there an easier way to get the domain of the user?
    ud = user_dir(basedir, user_id)
    res_creation_file = j(ud, "res_creation.json")
    if os.path.exists(res_creation_file):
        user_file = res_creation_file
    else:
        user_file = j(ud, "res_register.json")
    res_user = load_json_file(user_file)
    quid = res_user["response"]["content"]["qualified_id"]
    domain = quid["domain"]

    res_file = random_path(ud)
    res_post_client = save(post_clients(ctx), res_file)
    simple_expect_status(201, res_post_client)

    cid = {
        "client": res_post_client["response"]["content"]["id"],
        "user": user_id,
        "domain": domain,
    }

    cdir = client_dir(ud, cid["client"])
    os.makedirs(cdir, exist_ok=True)
    res_post_client_file = j(cdir, "res_post_client.json")
    shutil.move(res_file, res_post_client_file)
    print("Moving to", res_post_client_file)

    s = ClientState(cdir)
    s.client_identity = cid

    init_mls_client(s)
    pub = get_public_key(s)
    mls_public_keys = {"ed25519": b64encode(pub).decode("utf8")}
    res_public_keys = save(
        api.put_client_mls_public_keys(ctx, cid["client"], mls_public_keys),
        j(cdir, "res_public_keys.json"),
    )
    simple_expect_status(200, res_public_keys)

    res_upload_keypackage = save(
        upload_new_keypackage(ctx, s), j(cdir, "res_upload_keypackage.json")
    )
    simple_expect_status(201, res_upload_keypackage)

    return cid


def list_users(basedir):
    return os.listdir(j(basedir, "users"))


def list_regular_users(basedir):
    admin_id = meta_get(basedir, "admin_user_id")
    return [u for u in list_users(basedir) if u != admin_id]


def list_clients(user_dir):
    return os.listdir(j(user_dir, "clients"))


def defNewConvMLS(client_id):
    return {
        "name": "conv default name",
        "access": [],
        "conversation_role": "wire_admin",
        "creator_client": client_id,
        "protocol": "mls",
    }


def create_mls_conv(ctx, basedir):
    """
    Assumes admin is logged in
    """
    ud = admin_user_dir(basedir)

    # create conv
    client_id = list_clients(ud)[0]
    res_post_conv = save(
        api.create_conversation(ctx, user=None, **defNewConvMLS(client_id)),
        j(basedir, "res_post_conv.json"),
    )
    group_id = res_post_conv["response"]["content"]["group_id"]
    simple_expect_status(201, res_post_conv)

    cdir = client_dir(ud, client_id)
    state = ClientState.load(cdir)
    create_group(state, group_id)
    return state


def upgrade_access_token(ctx, basedir, user_id, client_id):
    token_upgrade = save(
        api.create_access_token(ctx, client_id=client_id),
        j(user_dir(basedir, user_id), "token_upgrade.json"),
    )
    simple_expect_status(200, token_upgrade)
    ctx.set_access_token(token_upgrade["response"]["content"]["access_token"])


def notifications_websocket_url(ctx, client_id):
    assert ctx.access_token is not None
    url = ctx.mkurl("cannon", "/await").replace("https://", "wss://")
    urlparts = list(urlparse(url))
    params = {"access_token": ctx.access_token, "client": client_id}
    urlparts[4] = urlencode(params)
    url = urlunparse(urlparts)
    return url


def epoch_millis():
    return time.time_ns() // (10**6)


class LogWithContext:
    def __init__(self, logger):
        self.context = {}
        self.logger = logger

    def log(self, msg, **kwargs):
        o = dict(self.context)
        o["msg"] = msg
        o["t"] = epoch_millis()
        o.update(kwargs)
        line = json.dumps(o)
        self.logger.info(line)


def main_setup_team(basedir=None, n_users=10):
    if basedir is None:
        basedir = tempfile.TemporaryDirectory().name

    os.makedirs(basedir, exist_ok=True)

    logfile = j(basedir, "setup.log")

    with create_logger(logfile) as (logfile, logger):

        print(basedir)
        print(f"Logging to {logfile}")

        log = LogWithContext(logger)
        log.log("setup_begin")

        ctx_admin = Context()
        admin_id = create_admin(ctx_admin, basedir)
        ctx_admin.switch_user(admin_id)
        save_public_keys(ctx_admin, basedir)
        admin_cid = create_mls_client(ctx_admin, basedir, admin_id)

        def create_client(i):
            ctx = copy.deepcopy(ctx_admin)
            ctx.switch_user(admin_id)

            print(f"======================== {i}")
            user_id = create_user(ctx, basedir)
            log.log("create_user", i=i, user_id=user_id)

            ctx.switch_user(user_id)
            cid = create_mls_client(ctx, basedir, user_id)

            ud = user_dir(basedir, user_id)
            res_register = load_json_file(j(ud, "res_register.json"))
            quid = res_register["response"]["content"]["qualified_id"]
            res_kp_claim = save(
                api.claim_key_packages(ctx, user=quid, target=quid),
                j(ud, "res_kp_claim.json"),
            )
            simple_expect_status(200, res_kp_claim)

        with concurrent.futures.ThreadPoolExecutor(max_workers=20) as executor:
            res = executor.map(create_client, range(n_users))

            # force iterator.
            list(res)

            # Somehow this doesn't surface the exception
            # while True:
            #     try:
            #         next(res)
            #     except StopIteration:
            #         break
            #     except Exception as e:
            #         print(e)
            #         chunk_size = 4
            #


def chunk(xs, chunk_size):
    for i in range(0, len(xs), chunk_size):
        yield xs[i : i + chunk_size]


def main_setup_add_participants(basedir, batch_size=300):
    ctx_admin = Context()
    admin_id = meta_get(basedir, "admin_user_id")

    admin_dir = user_dir(basedir, admin_id)

    admin_res_login = load_json_file(j(admin_dir, "res_login.json"))
    ctx_admin.load_cookies_from_response(admin_res_login)

    client_id = list_clients(admin_dir)[0]

    upgrade_access_token(ctx_admin, basedir, user_id=admin_id, client_id=client_id)

    client_state = create_mls_conv(ctx_admin, basedir)

    kpfiles = []

    users = [u for u in list_users(basedir) if u != admin_id]
    for user_id in users:
        ud = user_dir(basedir, user_id)

        try:
            res_kp_claim = load_json_file(j(ud, "res_kp_claim.json"))
        except:
            res_kp_claim = None
            continue

        if res_kp_claim["response"]["status_code"] != 200:
            print(res_kp_claim["response"]["status_code"])
            continue

        for kp in res_kp_claim["response"]["content"]["key_packages"]:
            ref = b64decode(kp["key_package_ref"])
            kpdata = b64decode(kp["key_package"])

            p = os.path.join(ud, ref.hex())
            with open(p, "wb") as f:
                f.write(kpdata)
            kpfiles.append(p)

    logfile = j(basedir, "setup_add_participants.log")

    with create_logger(logfile) as (logfile, logger):

        log = LogWithContext(logger)
        log.log("setup_begin")
        log.context = {"admin": 1}

        chunks = list(chunk(kpfiles, batch_size))

        for i, kpfiles in enumerate(chunks):

            print(f"{i+1} of {len(chunks)}")

            log.log("add_member_begin")
            message_package = add_member(client_state, kpfiles)
            log.log("add_member_end")

            log.log("make_bundle_begin")
            commit_bundle = make_bundle(message_package)
            log.log("make_bundle_end")

            log.log("post_commit_bundle_begin")
            ctx_admin.switch_user(admin_id)
            res_post_commit_bundle = save(
                api.mls_post_commit_bundle(ctx_admin, commit_bundle),
                j(admin_dir, "res_post_commit_bundle.json"),
            )
            log.log("post_commit_bundle_end")

            simple_expect_status(201, res_post_commit_bundle)


def main_send(basedir):
    logfile = j(basedir, "send.log")
    with create_logger(logfile) as (logfile, logger):
        print(f"Logging to {logfile}")
        ctx = Context()

        log = LogWithContext(logger)

        admin_id = meta_get(basedir, "admin_user_id")
        ctx.switch_user(admin_id)
        ud = user_dir(basedir, admin_id)

        res_login = load_json_file(j(ud, "res_login.json"))
        res = simplify_response(api.login(ctx, res_login["request"]["body"]["email"]))
        simple_expect_status(200, res)

        client_id = list_clients(ud)[0]
        cdir = client_dir(ud, client_id)
        admin_client_state = ClientState.load(cdir)

        log.context = {"admin": 1, "cid": cid2str(admin_client_state.client_identity)}

        msg = random_msg()
        log.log("message_send_begin")
        res_test_msg = save(
            api.mls_send_message(
                ctx, create_application_message(admin_client_state, msg)["message"]
            ),
            j(ud, "res_test_msg.json"),
        )
        log.log("message_send_end")
        simple_expect_status(201, res_test_msg)


def read_logfile(logfile):
    with open(logfile, "r") as f:
        for line in f:
            yield json.loads(line)


def mean(xs):
    return sum(xs) / len(xs)


def std(xs):
    e = mean(xs)
    ys = [(x - e) ** 2 for x in xs]
    return math.sqrt(sum(ys) / (len(xs) - 1))


def percentiles(xs, res=20):
    xs = sorted(xs)
    n = len(xs)
    p = 1
    result = {}
    for i, x in enumerate(xs):
        if i >= p / res * n:
            result[int(p * 100 // res)] = x
            p += 1
    l = 0
    for i in range(res):
        p = int(i * 100 // res)
        if result.get(p) is None:
            result[p] = l
        else:
            l = result[p]
    return result


def print_stats(name, xs):
    print(name)
    print(f"  n    : {len(xs)}")
    print(f"  mean : {mean(xs):0.0f}")
    print(f"  std  : {std(xs):0.0f}")
    print(f"  min  : {min(xs):0.0f}")
    print(f"  max  : {max(xs):0.0f}")
    percs = percentiles(xs)
    print(f"  10%  : {percs[10]}")
    print(f"  25%  : {percs[25]}")
    print(f"  50%  : {percs[50]}")
    print(f"  75%  : {percs[75]}")
    print(f"  90%  : {percs[90]}")
    print()


def main_analyze(basedir):
    from bokeh.plotting import figure, show, save

    logfiles = [j(basedir, fn) for fn in os.listdir(basedir) if fn.endswith(".log")]

    admin = {}
    cids = {}
    idxs = {}

    for logfile in logfiles:
        for d in read_logfile(logfile):
            if d.get("admin") == 1:
                admin[d["msg"]] = d["t"]
            else:
                cid = d.get("cid")
                if cid is not None:
                    if cids.get(cid) is None:
                        cids[cid] = {}

                    cids[cid][d["msg"]] = d["t"]
                    i = d.get("i")
                    user_id = d.get("user_id")
                    if i is not None and user_id is not None:
                        idxs[i] = cid

    post_commit_bundle_duration = []
    send_to_receive_duration = []

    for cid, d in cids.items():
        if d.get("post_commit_bundle_end") and d.get("post_commit_bundle_begin"):
            post_commit_bundle_duration.append(
                d["post_commit_bundle_end"] - d["post_commit_bundle_begin"]
            )
        if d.get("message_received"):
            send_to_receive_duration.append(
                d["message_received"] - admin["message_send_begin"]
            )

    if len(post_commit_bundle_duration) > 0:
        print_stats("post_commit_bundle_duration", post_commit_bundle_duration)

    if len(send_to_receive_duration) > 0:
        print_stats("send_to_receive_duration", send_to_receive_duration)

    if admin.get("message_send_end"):
        admin_message_send_duration = (
            admin["message_send_end"] - admin["message_send_begin"]
        )
        print(f"admin_message_send_duration: {admin_message_send_duration}")

    p = figure(
        title="Request duration for posting a commit bundle",
        x_axis_label="i",
        y_axis_label="ms",
    )
    p.line(
        list(range(len(post_commit_bundle_duration))),
        post_commit_bundle_duration,
        legend_label="post_commit_bundle_duration",
        color="blue",
        line_width=2,
    )
    show(p)


@contextmanager
def create_logger(logfile=None):
    if logfile is None:
        (fh, logfile) = tempfile.mkstemp()
        cm = os.fdopen(fh, "w")
    else:
        cm = open(logfile, "w")

    with cm as f:
        logger = setup_logger(f)
        yield (logfile, logger)


def setup_logger(f=None):
    """
    use create_logger()
    """
    logger = logging.Logger(random_letters())
    handler = logging.StreamHandler(f)
    handler.setFormatter(logging.Formatter("%(message)s"))
    logger.addHandler(handler)
    logger.setLevel(logging.INFO)
    return logger


def select_users(basedir, batch_index, batch_size):
    """
    args:
       batch_index: starting from 0
    """
    all_users = sorted(list_regular_users(basedir))
    users = all_users[batch_index * batch_size : (batch_index + 1) * batch_size]
    return users


async def main_receive(basedir, batch_index=0, batch_size=100):
    logfile = j(basedir, "receive.log")
    with create_logger(logfile) as (logfile, logger):
        print(f"Logging to logfile {logfile}")

        user_ids = select_users(basedir, batch_index=batch_index, batch_size=batch_size)
        stats = {"connected": 0, "received": 0, "n": len(user_ids)}

        async def receive_for_user(user_id):
            log = LogWithContext(logger)

            ctx = Context()

            ud = user_dir(basedir, user_id)
            res_login = load_json_file(j(ud, "res_login.json"))

            res = simplify_response(
                api.login(ctx, res_login["request"]["body"]["email"])
            )
            simple_expect_status(200, res)

            client_id = list_clients(ud)[0]

            upgrade_access_token(ctx, basedir, user_id, client_id)

            cdir = client_dir(ud, client_id)
            state_client = ClientState.load(cdir)

            log.context = {"cid": cid2str(state_client.client_identity)}

            url = notifications_websocket_url(ctx, client_id)

            log.log("started")

            await asyncio.sleep(random.random() * 0.3 * len(user_ids))

            async with websockets.connect(url, open_timeout=4 * 60) as ws:
                log.log("websocket_open")
                stats["connected"] += 1
                message = await ws.recv()
                # TODO: check for mls
                log.log("message_received")
                stats["received"] += 1

        async def report_stats():
            go = True
            while go:
                print(stats)
                await asyncio.sleep(1)
                go = stats["received"] < stats["n"]

        tasks = []
        for user_id in user_ids:
            task = asyncio.create_task(receive_for_user(user_id))
            tasks.append(task)
        tasks.append(asyncio.create_task(report_stats()))

        for task in tasks:
            await task

        print(stats)


# NOTE: unused
def fetch_notifications(ctx, state):
    get_page = True
    last = state.last_notification

    result = []

    while get_page:
        if last is not None:
            since = str(last)
        else:
            since = None
        res_notifs = save(
            get_notifications(ctx, since=since, client=state.client_identity["client"]),
            j(state.client_dir, "res_notifs.json"),
        )
        simple_expect_status(200, res_notifs)
        body = res_notifs["response"]["content"]
        for n in body["notifications"]:
            notif_id = UUID(n["id"])
            if last is None:
                last = notif_id
            last = max(last, notif_id, key=lambda u: u.time)

            assert len(n["payload"]) == 1
            payload = n["payload"][0]

            result.append(payload)

        get_page = body["has_more"]

    state.last_notification = last
    return result


# NOTE: unused
def client_consume_notifications(ctx, state, basedir):
    n_msgs = 0
    notifications = fetch_notifications(ctx, state)

    removal_key_file = j(basedir, "removal_key.json")

    result = []

    for payload in notifications:
        if payload["type"] == "conversation.mls-welcome":
            msg_data = b64decode(payload["data"])
            consume_welcome(state, msg_data)

        if payload["type"] == "conversation.mls-message-add":
            msg_data = b64decode(payload["data"])
            out = consume_message(state, msg_data, removal_key_file, ignore_stale=True)
            result.append(out)
            n_msgs += 1

    return result


async def main():
    parser = argparse.ArgumentParser(
        prog=sys.argv[0], description="Performance tester for MLS"
    )
    subparsers = parser.add_subparsers(
        title="subcommand", description="valid subcommands", dest="subparser_name"
    )

    subparsers.add_parser("setup")
    subparsers.add_parser("add").add_argument("basedir", type=str)
    subparsers.add_parser("send").add_argument("basedir", type=str)
    subparsers.add_parser("receive").add_argument("basedir", type=str)
    subparsers.add_parser("analyze").add_argument("basedir", type=str)

    args = parser.parse_args()

    if args.subparser_name == "setup":
        main_setup_team()

    elif args.subparser_name == "add":
        main_setup_add_participants(args.basedir)

    elif args.subparser_name == "send":
        main_send(args.basedir)

    elif args.subparser_name == "receive":
        await main_receive(args.basedir)

    elif args.subparser_name == "analyze":
        main_analyze(args.basedir)

    else:
        parser.print_usage()


if __name__ == "__main__":
    asyncio.run(main())
