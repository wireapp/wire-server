#!/usr/bin/env python3
#
# This file is part of the Wire Server implementation.
#
# Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU Affero General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option) any
# later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
# details.
#
# You should have received a copy of the GNU Affero General Public License along
# with this program. If not, see <https://www.gnu.org/licenses/>.

from __future__ import print_function

# NOTE: This python script requires the "requests" library to be installed.

# Change this if you are running your own instance of Wire.
BACKEND_URL='https://prod-nginz-https.wire.com'

import sys
import getpass
from requests import Request, Session
import requests
import json
import datetime

session = None

def init_session():
    global session
    session = Session()
    session.headers.update({'User-Agent': "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/87.0.4280.88 Safari/537.36"})

def post(url):
    return Request('POST', url)

def get(url):
    return Request('GET', url)

def set_bearer_token(request, token):
    request.headers['Authorization'] = 'Bearer ' + token

def backend(path):
    return BACKEND_URL + path

def has_json_content(response):
    content_type = response.headers.get('Content-Type')
    if content_type is not None:
        return (content_type.startswith('application/json')
                 or content_type == 'application/scim+json;charset=utf-8')
    else:
        return False

def send_request(session, request):
    response = session.send(session.prepare_request(request))
    if 200 <= response.status_code and response.status_code < 300:
        if has_json_content(response):
            return response.json()
        else:
            return response
    else:
        print(f"Request failed {request.url}", file=sys.stderr)
        if has_json_content(response):
            tpl = response, response.json()
        else:
            tpl = response, response.content
        print(tpl, file=sys.stderr)
        exit(1)

def create_bearer(email, password):
    r = post(backend('/login?persist=false'))
    r.headers['Accept'] = 'application/json'
    r.json = {'email': email, 'password': password}
    return send_request(session, r)

def create_scim_token(admin_password, token):
    r = post(backend('/scim/auth-tokens'))
    set_bearer_token(r, token)
    r.json = {'description': 'token generated at ' + datetime.datetime.now().isoformat(),
              'password': admin_password
              }
    return send_request(session, r)

def exit_fail(msg):
    print(msg, file=sys.stderr)
    exit(1)

def main():
    init_session()
    print('This script generates a token that authorizes calls to Wire\'s SCIM endpoints.\n')
    print('Please enter the login credentials of a user that has role "owner" or "admin".')
    ADMIN_EMAIL=input("Email: ") or exit_fail('Please provide an email.')
    ADMIN_PASSWORD=getpass.getpass('Password: ') or exit_fail('Please provide password.')
    bearer_token = create_bearer(ADMIN_EMAIL, ADMIN_PASSWORD)
    scim_token = create_scim_token(ADMIN_PASSWORD, bearer_token['access_token'])
    print('Wire SCIM Token: ' + scim_token['token'])
    print('The token will be valid until you generate a new token for this user.')

if __name__ == '__main__':
    main()
