from helpers import setup
from helpers.conversions import *

def test_sender_not_in_conv(ctx, mls):
    alice, bob = setup.connected_users((ctx, 2))
    alice1 = mls.create_client(alice)
    bob1 = mls.create_client(bob)

    # create group with alice1 and bob1, but do not commit adding bob
    mls.upload_new_key_package(bob1)
    mls.setup_group(alice1)
    mp = mls.create_add_commit(alice1, [bob])
    mls.consume_welcome(mp.welcome)

    msg = mls.create_application_message(bob1, "some text")
    with ctx.mls_message(bob, msg.message) as r:
        assert r.status_code == 404
        assert r.json()['label'] == 'no-conversation'

def test_another_user_commit(ctx, mls):
    alice, bob = setup.connected_users((ctx, 2))
    alice1 = mls.create_client(alice)
    bob1 = mls.create_client(bob)
    mls.upload_new_key_package(bob1)
    mls.setup_group(alice1)
    mls.send_and_consume_commit(mls.create_add_commit(alice1, [bob]))

    # alice creates a commit that adds bob2
    bob2 = mls.create_client(bob)
    mls.upload_new_key_package(bob2)
    mp = mls.create_add_commit(alice1, [bob])

    # which is then sent by bob instead of alice
    with ctx.mls_message(bob, mp.message) as r:
        assert r.status_code == 400
        assert r.json()['label'] == 'mls-client-sender-user-mismatch'

def test_welcome(ctx, mls):
    alice, bob = setup.connected_users((ctx, 2))
    alice1 = mls.create_client(alice)
    bob1 = mls.create_client(bob)
    mls.upload_new_key_package(bob1)
    mls.setup_group(alice1)
    mp = mls.create_add_commit(alice1, [bob])
    assert mp.welcome, "expected welcome message"
    with setup.ws_connect_users(ctx, bob1) as ws:
        mls.send_and_consume_commit(mp)

        qid = obj_qid(bob)
        e = ws.match(type='conversation.mls-welcome')
        assert not e['transient']
        assert e['conversation'] == qid['id']
        assert e['qualified_from'] == qid

def test_welcome_no_key(ctx, mls):
    alice, bob = setup.connected_users((ctx, 2))
    alice1 = mls.create_client(alice)
    bob1 = mls.create_client(bob)
    mls.setup_group(alice1)

    # add bob using an "out-of-band" key package
    kp = mls.generate_key_package(bob1)
    mp = mls.create_add_commit(alice1, extra_key_packages={bob1: kp})
    assert mp.welcome, "expected welcome message"

    with ctx.mls_welcome(alice, mp.welcome) as r:
        assert r.status_code == 404
        assert r.json()['label'] == "mls-key-package-ref-not-found"

def test_remote_welcome(ctx, ctx2, mls):
    alice, bill = setup.connected_users((ctx, 1), (ctx2, 1))

    alice1 = mls.create_client(alice)
    bill1 = mls.create_client(bill)
    mls.upload_new_key_package(bill1)

    mls.setup_group(alice1)
    mp = mls.create_add_commit(alice1, [bill])
    assert mp.welcome, "expected welcome message"

    with setup.ws_connect_users(ctx2, bill1) as ws:
        mls.send_and_consume_commit(mp)
        e = ws.match(type='conversation.mls-welcome')
        assert e['conversation'] == obj_id(bill)
        assert e['qualified_from'] == obj_qid(bill)
