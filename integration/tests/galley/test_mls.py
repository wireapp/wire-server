from helpers import setup

def test_sender_not_in_conv(mls):
    alice, bob = setup.connected_users(mls.ctx, 2)
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

def test_another_user_commit(mls):
    alice, bob = setup.connected_users(mls.ctx, 2)
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
    with mls.ctx.mls_message(bob, mp.message) as r:
        assert r.status_code == 400
        assert r.json()['label'] == 'mls-client-sender-user-mismatch'
