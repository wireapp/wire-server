"""This module aims to abstract calls to intra and client API in such a generic
way that it can be used in multiple contexts. Generality is achieved via the
`ctx` argument. Use either context.py or overwrite with a custom Context.
"""

from base64 import b64encode
import time
import json
import random
import requests
import string
from .conversions import obj_qid, obj_id, obj_path

DEFAULT_PASSWORD = "hunter2!"


def random_letters(n=10):
    return "".join(random.choices(string.ascii_letters, k=n))


def random_email(domain='example.com'):
    return "test-email" + "-" + random_letters(10) + "@" + domain


def create_user(ctx, email=None, password=None, name=None, create_team=False, **kwargs):
    if email is None:
        email = random_email()

    if password is None:
        password = DEFAULT_PASSWORD

    if name is None:
        name = email

    body = {"email": email, "password": password, "name": name}
    if create_team:
        body["team"] = {"name": "Wire team 2", "icon": "default"}

    for k, v in kwargs:
        body[k] = v

    url = ctx.mkurl("brig", "/i/users", internal=True)
    return ctx.request("POST", url, json=body)


def add_client(
    ctx,
    user,
    pks,
    lpk,
    *,
    internal=False,
    ctype=None,
    label="Test Device",
    model="Test Model",
    password=DEFAULT_PASSWORD,
):
    body = {
        "prekeys": pks,
        "lastkey": lpk,
        "type": ctype,
        "label": label,
        "model": model,
        "password": password,
    }
    if ctype == "legalhold":
        body["class"] = "legalhold"
    if internal:
        url = f"/i/clients/{obj_id(user)}"
    else:
        url = "/clients"

    url = ctx.mkurl("brig", url, internal=internal)
    return ctx.request("POST", url, user=user, json=body)


def put_client_mls_public_keys(ctx, client_id, mls_public_keys):
    body = {"mls_public_keys": mls_public_keys}
    url = ctx.mkurl("brig", f"/clients/{client_id}")
    return ctx.request("PUT", url, json=body)


def delete_client(ctx, user, client_id, password=DEFAULT_PASSWORD):
    url = ctx.mkurl("brig", f"/clients/{obj_id(user)}")
    return ctx.request("DELETE", url, user=user, json={"password": password})


def get_clients_unqualified(ctx, user, **kwargs):
    url = ctx.mkurl("brig", "/clients")
    return ctx.request("GET", url, user=user)


def get_user_clients(ctx, user, target):
    url = ctx.mkurl("brig", f"/users/{obj_path(target)}/clients")
    return ctx.request("GET", url, user=user)


def create_connection(ctx, user, target):
    url = ctx.mkurl("brig", f"/connections/{obj_path(target)}")
    return ctx.request("POST", url, user=user)


def update_connection(ctx, user, target, status):
    url = ctx.mkurl("brig", f"/connections/{obj_path(target)}")
    return ctx.request("PUT", url, user=user, json={"status": status})


def create_conversation(ctx, user, users=None, **kwargs):
    data = {"qualified_users": [obj_qid(u) for u in (users or [])], **kwargs}
    url = ctx.mkurl("galley", "/conversations")
    return ctx.request("POST", url, user=user, json=data)


def get_conversation(ctx, user, conv):
    url = ctx.mkurl("galley", f"/conversations/{obj_path(conv)}")
    return ctx.request("GET", url, user=user)


def mls_get_public_keys(ctx, **additional_args):
    url = ctx.mkurl("galley", "/mls/public-keys")
    return ctx.request("GET", url)


def mls_message(ctx, user, message):
    headers = {"Content-Type": "message/mls"}
    url = ctx.mkurl("galley", "/mls/messages")
    return ctx.request("POST", url, user=user, data=message, headers=headers)


def mls_welcome(ctx, user, welcome):
    headers = {"Content-Type": "message/mls"}
    url = ctx.mkurl("galley", "/mls/welcome")
    return ctx.request("POST", url, user=user, data=welcome, headers=headers)


def mls_post_commit_bundle(ctx, client, commit_bundle):
    url = ctx.mkurl("galley", f"/mls/commit-bundles")
    tbefore = time.time()
    res = ctx.request(
        "POST",
        url,
        headers={"Content-Type": "message/mls"},
        client=client,
        data=commit_bundle,
    )
    tafter = time.time()
    print(f'posting commit bundle took {tafter-tbefore:.0f} seconds')
    return res


def mls_send_message(ctx, msg, **kwargs):
    headers = {"Content-Type": "message/mls"}
    url = ctx.mkurl("galley", f"/mls/messages")
    return ctx.request("POST", url, headers=headers, data=msg, **kwargs)


def upload_key_packages(ctx, user, client, key_packages):
    url = ctx.mkurl("brig", f"/mls/key-packages/self/{client}")
    data = {"key_packages": [b64encode(kp).decode("ascii") for kp in key_packages]}
    return ctx.request("POST", url, user=user, json=data)


def claim_key_packages(ctx, user, target, **kwargs):
    url = ctx.mkurl("brig", f"/mls/key-packages/claim/{obj_path(target)}")
    return ctx.request("POST", url, user=user)


def remove_member(ctx, user, conv, target, **kwargs):
    url = ctx.mkurl(
        "galley", f"/conversations/{obj_path(conv)}/members/{obj_path(target)}"
    )
    return ctx.request("DELETE", url, user=user)


def login(ctx, email, password=DEFAULT_PASSWORD, **additional_args):
    body = {"email": email, "password": password}
    url = ctx.mkurl("brig", "/login")
    return ctx.request("POST", url, json=body)


def create_access_token(ctx, client_id=None):
    """
    This functions expects that ctx adds cookies
    """
    params = {}
    if client_id is not None:
        params["client_id"] = client_id
    url = ctx.mkurl("brig", "/access")
    return ctx.request("POST", url, params=params)


def create_team_invitation(ctx, team, email_invite=None, user=None, **additional_args):
    if email_invite is None:
        email_invite = random_email()
    url = ctx.mkurl("brig", f"/teams/{team}/invitations")
    return ctx.request(
        "POST",
        url,
        json={"email": email_invite, "name": "Replace with name"},
        user=user,
    )


def get_invitation_code(ctx, team, invitation, **kwargs):
    url = ctx.mkurl("brig", "/i/teams/invitation-code", internal=True)
    params = {"team": team, "invitation_id": invitation}
    return ctx.request("GET", url, params=params, **kwargs)


def register_user(ctx, email, code, password=DEFAULT_PASSWORD, **kwargs):
    body = {"name": "Bob", "email": email, "password": password, "team_code": code}
    url = ctx.mkurl("brig", "/register")
    return ctx.request("POST", url, json=body, **kwargs)
