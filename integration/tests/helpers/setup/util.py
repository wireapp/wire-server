import itertools

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

def connected_users(*num_by_ctx):
    users = [(ctx, random_user(ctx)) for ctx, num in num_by_ctx
             for _ in range(num)]

    for (ctx1, u1), (ctx2, u2) in itertools.combinations(users, 2):
        ctx1.create_connection(u1, u2).check(status=201)
        ctx2.update_connection(u2, u1, 'accepted').check(status=200)

    return [u for _, u in users]
