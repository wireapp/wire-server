from helpers import setup

def test_sender_not_in_conv(ctx, mls):
    # create users
    alice, bob = setup.connected_users(ctx, 2)

    # create clients
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
