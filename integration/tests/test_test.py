from helpers import api

ctx = api.Context({'brig': 8082})

def test_brig_user():
    with api.create_user(ctx) as r:
        assert r.status_code == 201
