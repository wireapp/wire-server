#!/usr/bin/env python3

from wire import api
from wire.context import Context
import csv
import argparse

SCIM_TOKEN = ""

def get_scim_user(ctx, user_id):
    url = ctx.mkurl("spar", f"scim/v2/Users/{user_id}")
    return ctx.request('GET', url, headers=({'Authorization': f'Bearer {SCIM_TOKEN}'}))

def post_scim_user(ctx, user_id, body):
    url = ctx.mkurl("spar", f"scim/v2/Users/{user_id}")
    return ctx.request('POST', url, headers=({'Authorization': f'Bearer {SCIM_TOKEN}'}), json=body)

def main():
    ctx = Context(domain="dummy", version="4", service_map={'spar': 8081})
    user_id = '49d94bf5-b0ec-467f-a6b8-0fad7874180d'
    r =  get_scim_user(ctx, user_id)
    assert r.response.status_code == 200
    body = dict(r.json())
    body['externalId'] = 'stefan+test99999@wire.com'

    r2 = post_scim_user(ctx, user_id, body)
    return r2
