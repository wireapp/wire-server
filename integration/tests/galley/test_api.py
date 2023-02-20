from helpers import setup

def test_status(ctx):
    url = ctx.mkurl('galley', '/i/status', internal=True)
    with ctx.request('GET', url) as r:
        assert r.status_code == 200
    with ctx.request('HEAD', url) as r:
        assert r.status_code == 200

def test_metrics(ctx):
    with ctx.request('GET', ctx.mkurl('galley', '/i/metrics', internal=True)) as r:
        assert r.status_code == 200
        assert "TYPE http_request_duration_seconds histogram" in r.text

def test_get_conv_v2(ctx):
    alice, bob = setup.connected_users(ctx, 2)
    with ctx.create_conversation(alice, users=[bob]) as r:
        assert r.status_code == 201
        conv = r.json()

    with ctx.get_conversation(alice, conv) as r:
        assert r.status_code == 200
        assert conv == r.json()
