from ..prekeys import Prekeys
from .. import api
from . import util
from ..conversions import *

import base64
import dataclasses
import json
import tempfile
import os
import subprocess

@dataclasses.dataclass(frozen=True)
class MessagePackage:
    sender: object
    message: bytes
    welcome: bytes = None
    public_group_state: bytes = None

@dataclasses.dataclass
class ClientState:
    directory: str
    group: bytes = None

@dataclasses.dataclass(frozen=True)
class ClientIdentity:
    user: object
    client: str

    def __str__(self):
        return f"{self.user['id']}:{self.client}@{self.user['domain']}"

class MLS:
    def __init__(self, ctx, directory):
        self.ctx = ctx
        self.prekeys = Prekeys.new()
        self.state = {}
        self.directory = directory
        self.new_members = []

    def __getitem__(self, cid):
        return self.state[cid]

    def temp(self):
        return tempfile.NamedTemporaryFile(dir=self.directory, delete=False)

    def create_client(self, user):
        # create wire client
        client = util.create_client(self.ctx, user, self.prekeys)
        cid = ClientIdentity(obj_qid(user), client)
        print('created client', cid)

        # setup client state
        directory = os.path.join(self.directory, str(cid))
        self.state[cid] = ClientState(directory)

        # setup mls client
        self.cli(cid, "init", str(cid))

        # upload public key
        pk = base64.b64encode(self.cli(cid, "public-key")).decode('ascii')
        self.ctx.request('PUT', self.ctx.mkurl('brig', f'/clients/{client}'),
                         headers=api.std_headers(cid.user),
                         json={'mls_public_keys': {'ed25519': pk}}) \
                .check(status=200)

        return cid

    def upload_new_key_package(self, cid):
        print('upload', cid)
        kp = self.cli(cid, "key-package", "create")
        self.ctx.upload_key_packages(cid.user, cid.client, [kp]).check(status=201)

    def claim_key_packages(self, cid, user):
        r = self.ctx.claim_key_packages(cid.user, user).check(status=200).json()
        for e in r['key_packages']:
            cid1 = ClientIdentity(qid(e['domain'], e['user']), e['client'])
            yield cid1, base64.b64decode(e['key_package'])

    def setup_group(self, cid):
        conv = self.ctx.create_conversation(cid.user, 
                                            protocol='mls',
                                            creator_client=cid.client) \
                    .check(status=201).json()
        assert conv['protocol'] == 'mls'
        group_id = conv['group_id']
        group = json.loads(self.cli(cid, "group", "create", group_id))
        self[cid].group = group
        json.dumps(group, indent=2)

    def key_package_file(self, kp):
        with self.temp() as kpf:
            kpf.write(kp)
            return kpf.name

    def create_add_commit(self, cid, users):
        # note: these files are saved in the sender's client dir
        welcome_file = self.temp()
        pgs_file = self.temp()

        key_packages = {c: self.key_package_file(kp) for user in users 
                        for c, kp in self.claim_key_packages(cid, user)}
        assert len(key_packages) > 0

        msg = self.cli(cid, 
                "member", "add", "--group", "<group-in>",
                "--welcome-out", welcome_file.name,
                "--group-state-out", pgs_file.name,
                "--group-out", "<group-out>", 
                *key_packages.values())
        self.new_members.extend(key_packages.keys())

        welcome = welcome_file.read()
        pgs = pgs_file.read()

        return MessagePackage(
            sender=cid,
            message=msg,
            welcome=welcome,
            public_group_state=pgs)

    def create_application_message(self, cid, text):
        msg = self.cli(cid, "message", "--group", "<group-in>", text)

        return MessagePackage(sender=cid, message=msg)

    def consume_welcome(self, welcome):
        for cid in self.new_members:
            state = self[cid]
            assert state.group is None
            self.cli(cid,
                "group", "from-welcome", "--group-out", "<group-out>", "-",
                stdin=welcome)
        self.new_members = []

    def cli(self, cid, *args, stdin=None):
        state = self[cid]

        subst = {}

        group_in_file = None
        if '<group-in>' in args:
            group_in_file = tempfile.NamedTemporaryFile(mode='w+', dir=self.directory, delete=False)
            assert state.group is not None
            json.dump(state.group, group_in_file)
            group_in_file.flush()
            subst['<group-in>'] = group_in_file.name

        group_out_file = None
        if '<group-out>' in args:
            group_out_file = tempfile.NamedTemporaryFile(dir=self.directory, delete=False)
            subst['<group-out>'] = group_out_file.name

        args_substd = []
        for arg in args:
            arg_substd = subst.get(arg)
            if arg_substd is not None:
                args_substd.append(arg_substd)
            else:
                args_substd.append(str(arg))

        all_args = ['mls-test-cli', '--store',
                    os.path.join(state.directory, str(cid), 'store')] + \
                            args_substd

        p = subprocess.Popen(all_args, stdout=subprocess.PIPE,
                             stdin=subprocess.PIPE if stdin is not None else None,
                             stderr=subprocess.PIPE)

        stdout_data, stderr_data = p.communicate(input=stdin)

        if group_in_file is not None: 
            del group_in_file

        if p.returncode != 0:
            msg = f'mls-test-cli failed returned status {p.returncode}\n'
            msg += f'=== command line: {" ".join(all_args)}\n'
            msg += '=== stdout:\n'
            msg += stdout_data.decode('utf8')
            msg += '=== stderr:\n'
            msg += stderr_data.decode('utf8')
            raise ValueError(msg)

        if group_out_file is not None:
            state.group = json.loads(group_out_file.read())
            del group_out_file

        return stdout_data
