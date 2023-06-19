#!/usr/bin/env python3

# Repair pending email confirmations after scim email update
#
# When updating email addresses (ie., externalIds) via scim in a team
# where users should have their email addresses validated, when a user
# doesn't follow up on the confirmation email (aka validation email),
# after expiration of the validation credentials, the system goes into
# an undesired state:
#
# ```
# brig.user.email = <old, valid address>
# brig.user.unvalidated_email = null
# spar.scim_external.external_id = <new, unvalidated email address>
# ```
#
# There is no way for the team/scim admins to recover from this state,
# since a scim update is ignored and doesn't send a new confirmation
# email.
#
# If you have this problem, AND YOU HAVE CONFIRMED INTERFERING DOING
# THIS IS LEGAL AND EITHER HAS OR DOES NOT LEGALLY REQUIRE THE CONSENT
# OF THE AFFECTED USERS, then you can run this script instead.
#
# Start by editing Section `configure this section` to your taste.  You
# may need to create a new scim token for this, and
# https://docs.wire.com/understand/single-sign-on/understand/main.html#using-scim-via-curl
# may prove useful in that if you are customer support and have no
# account in the team.  Remember to clean up after yourself and remove
# the token when you're done!
#
# `filename` should consist of rows of the form
# `old_email,user_id,new_email`.
#
# The script goes through this file, and for every user: (1) changes the
# email address to `old_email` and emulates the confirmation flow (the
# user doesn't have to do anything); (2) then changes the email address
# to `new_email`, again emulating the confirmation flow.  (1) is needed
# because changing to the new email twice will cause spar to notice that
# the external_id is already `new_email`, and doesn't do anything.

from wire import api
from wire.context import Context
import csv
import time # for `time.sleep`.  FUTUREWORK: you can probably tweak the
            # sleeps we've injected a lot, or remove all of them,
            # without doing any harm.  just test it on staging first!
import argparse

### configure this section

scim_token = "..."
filename = './test.csv'
ctx = None

### brig, spar api

def get_scim_user(ctx, user_id):
    url = ctx.mkurl("spar", f"scim/v2/Users/{user_id}")
    return ctx.request('GET', url, headers=({'Authorization': f'Bearer {scim_token}'}))

def get_brig_user(ctx, user_id):
    url = ctx.mkurl("brig", "self")
    return ctx.request('GET', url, headers=({'Z-User': user_id}))

def put_scim_user(ctx, user_id, body):
    url = ctx.mkurl("spar", f"scim/v2/Users/{user_id}")
    return ctx.request('PUT', url, headers=({'Authorization': f'Bearer {scim_token}'}), json=body)

def get_activation_code(ctx, user_id, email):
    url = ctx.mkurl("brig", f"i/users/activation-code", internal=True)
    return ctx.request('GET', url, params=({'email': email}))

def confirm_new_email(ctx, user_id, key, code):
    url = ctx.mkurl("brig", f"/activate")
    return ctx.request('GET', url, headers=({'Z-User': user_id}), params=({'key': key, 'code': code}))

def assert_resp(resp, status_want):
    if resp.response.status_code == status_want:
        return
    else:
        print(resp.text);
        # FUTUREWORK: it's not necessary to fail here; we could also
        # printf this and continue with the next row of the input;
        # later collecting all the failing cases into a list to be
        # handed back to the team admin for further analysis.
        assert False

### idioms

def confirm_email(user_id, email):
    r = get_activation_code(ctx, user_id, email)
    assert_resp(r, 200)
    r2 = confirm_new_email(ctx, user_id, r.json()['key'], r.json()['code'])
    assert_resp(r2, 200)
    return r2

# update from one email address to another.  this is called twice,
# first for "to old address" to make the db state consistent, and then
# for "to new address".  it's possible that this should be two
# functions, but so far they have no difference to suggest that.
def update(user_id, email):
    r = get_scim_user(ctx, user_id)
    assert_resp(r, 200)
    body = dict(r.json())
    if body['externalId'] != email:
        body['externalId'] = email
        r2 = put_scim_user(ctx, user_id, body)
        body2 = r2.json()
        # FUTUREWORK: there are some legitimate ways to fail in the
        # following line, if the email address is already the old one.
        # add code here to your liking in order to handle these more
        # gracefully.  you may want to distinguish between "change to
        # old address to make everything consistent" (where it's ok if
        # you find that it's already consistent, and you don't need to
        # do anything) and "change to new email address" (where you
        # certainly want a response that says "yes, it worked").
        assert_resp(r2, 200)
        return True
    else:
        report_state('old=new', user_id)
        return False

def report_state(msg, user_id):
    r = get_scim_user(ctx, user_id)
    assert_resp(r, 200)
    r2 = get_brig_user(ctx, user_id)
    assert_resp(r2, 200)
    print(f"[{msg}] uid={user_id}; scim_email={r.json()['externalId']}; brig_email={r2.json()['email']}")
    return r.json()['externalId'], r2.json()['email']


### process one item

def update_back_and_forth(user_id, old_email, new_email):
    assert old_email != new_email

    # nothing has happened yet
    email_scim, email_brig = report_state('before', user_id)

    # change to old email address to unblock pending confirmation
    # if brig already has the old email, setting to old email won't yield an activation code, so we don't.
    if update(user_id, old_email):
        if email_brig != old_email:
            time.sleep(4)
            confirm_email(user_id, old_email)
    report_state('between', user_id)

    # change back to new email address
    if update(user_id, new_email):
        confirm_email(user_id, new_email)
    else:
        # nothing to confirm; logging happens in update_to_new
        pass
    report_state('after', user_id)

### process csv file

def main():
    global ctx
    with open(filename, newline='') as csvfile:
        rows = csv.reader(csvfile, delimiter=',')
        for row in rows:
            ctx = Context(domain="localhost", version="4", service_map={'spar': 8081, 'brig': 8082})
            [old_email, user_id, new_email] = row
            update_back_and_forth(user_id, old_email, new_email)
            time.sleep(5)

if __name__ == '__main__':
   main()
