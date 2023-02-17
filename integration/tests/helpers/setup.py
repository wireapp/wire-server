"""Higher level utilities. Useful for setting up tests without carefully
checking that the endpoints used are working correctly."""

def random_user(ctx, **kwargs):
    with ctx.create_user(**kwargs) as r:
        assert r.status_code == 201
        return r.json()

def create_client(ctx, uid, prekeys):
    with ctx.add_client(uid, [prekeys.pop()], prekeys.lpop(), ctype='permanent') as r:
        assert r.status_code == 201
        return r.json()['id']
