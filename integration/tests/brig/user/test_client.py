from helpers import setup
import json

def test_cant_delete_lh_client(ctx, prekeys):
    uid = setup.random_user(ctx)['id']

    with ctx.add_client(uid, [prekeys.pop()], prekeys.lpop(), 
                        ctype="legalhold", internal=True) as r:
        assert r.status_code == 201
        resp = r.json()
    lh_client_id = resp['id']

    with ctx.delete_client(uid, lh_client_id) as r:
        assert r.status_code == 400

def test_cant_add_lh_client(ctx, prekeys):
    uid = setup.random_user(ctx)['id']
    # regular users cannot add legalhold clients
    with ctx.add_client(uid, [prekeys.pop()], prekeys.lpop(), ctype='legalhold') as r:
        assert r.status_code == 400

def test_get_user_clients_unqualified(ctx, prekeys):
    uid = setup.random_user(ctx)['id']
    ids = set(setup.create_client(ctx, uid, prekeys) for _ in range(3))
    
    with ctx.get_clients_unqualified(uid) as r:
        assert r.status_code == 200
        assert set(c['id'] for c in r.json()) == ids

def test_get_user_clients(ctx, prekeys):
    user1 = setup.random_user(ctx)
    user2 = setup.random_user(ctx)

    ids = set(setup.create_client(ctx, user1, prekeys) for _ in range(3))
    with ctx.get_user_clients(user2, user1) as r:
        r.status_code == 200
        assert set(c['id'] for c in r.json()) == ids
