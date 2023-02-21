from helpers import setup, assertions, conversions

def test_status(ctx):
    url = ctx.mkurl('galley', '/i/status', internal=True)
    with ctx.request('GET', url) as r:
        assert r.status_code == 200
    with ctx.request('HEAD', url) as r:
        assert r.status_code == 200

def test_metrics(ctx):
    url = ctx.mkurl('galley', '/i/metrics', internal=True)
    with ctx.request('GET', url) as r:
        assert r.status_code == 200
        assert "TYPE http_request_duration_seconds histogram" in r.text

def test_get_conv_v2(ctx):
    alice, bob = setup.connected_users(ctx, 2)
    with ctx.create_conversation(alice, users=[bob]) as r:
        assert r.status_code == 201
        conv = conversions.conv_v2(r.json())

    with ctx.versioned(2).get_conversation(alice, conv) as r:
        assert r.status_code == 200
        assert conv == r.json()

def test_create_proteus_conv(ctx):
    users = setup.connected_users(ctx, 3)
    alice, bob, jane = users

    name = "a" * 256
    with setup.ws_connect_users(ctx, bob, jane) as ws:
        with ctx.create_conversation(alice, users=[bob, jane], name=name) as r:
            assert r.status_code == 201
            conv = r.json()

        assertions.conversation(conv, creator=alice['id'], name=name, members=[bob, jane])

        for user in (bob, jane):
            e = ws.match(lambda e: e['qualified_conversation'] == conv['qualified_id'],
                          user=user)

            with ctx.get_conversation(user, conv) as r:
                r.status_code == 200
                conv_view = conversions.conv_v2(r.json())

            assert not e['transient']
            assert e['type'] == 'conversation.create'
            assert e['qualified_from'] == alice['qualified_id']
            assert conversions.conv_canonical(e['data']) == \
                    conversions.conv_canonical(conv_view)
