#!/usr/bin/env python3

import sys
import argparse

from wire import api
from wire.context import Context


def invite_user(ctx, admin_user_id, team):
    r = api.create_team_invitation(ctx, team, user=admin_user_id)
    assert r.response.status_code == 201
    res_inv = r.response.json()
    invitation = res_inv['id']
    email = res_inv['email']

    r2 = api.get_invitation_code(ctx, team, invitation, user=admin_user_id)
    assert r2.response.status_code == 200
    code = r2.response.json()['code']

    r3 = api.register_user(ctx, email=email, code=code)
    assert r3.response.status_code == 201
    password = r3.request['json']['password']
    user_id = r3.response.json()['id']

    print('user_id', user_id)
    print('email', email)
    print('password', password)


def main():
    parser = argparse.ArgumentParser(
        prog=sys.argv[0], description="Create team and members via brig's internal api"
    )
    parser.add_argument('brig_port')
    parser.add_argument('-n', '--number-users', default='1')
    args = parser.parse_args()
    local_brig_port = int(args.brig_port)
    n_users = int(args.number_users)

    ctx = Context(domain="dummy", version="3", service_map={'brig': local_brig_port})
    create_user = api.create_user(ctx, create_team=True)

    admin_email = create_user.request['json']['email']
    admin_password = create_user.request['json']['password']
    team = create_user.response.json()['team']
    admin_user_id = create_user.response.json()['id']

    print('# Team')
    print('team', team)
    print()

    print('# Admin')
    print('admin_user_id', admin_user_id)
    print('admin_email', admin_email)
    print('admin_password', admin_password)
    print()

    for i in range(n_users):
        print(f'# Member {i+1}')
        invite_user(ctx, admin_user_id=admin_user_id, team=team)
        print()


if __name__ == '__main__':
    main()
