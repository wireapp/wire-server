def test_brig_user(ctx):
    with ctx.create_user() as r:
        assert r.status_code == 201
