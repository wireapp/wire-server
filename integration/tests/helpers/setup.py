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

def connected_users(ctx, num):
    users = [random_user(ctx) for _ in range(num)]

    for u1, u2 in itertools.combinations(users, 2):
        ctx.create_connection(u1, u2)
        ctx.update_connection(u2, u1, 'accepted')

    return users
