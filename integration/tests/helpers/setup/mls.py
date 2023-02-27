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
    group: object = None

@dataclasses.dataclass(frozen=True)
class ClientIdentity:
    user: object
    client: str

    def __str__(self):
        return f"{self.user['id']}:{self.client}@{self.user['domain']}"

class MLS:
    def __init__(self, directory, *ctxs):
        self.ctxs = {ctx.domain:ctx for ctx in ctxs}
        self.prekeys = Prekeys.new()
        self.state = {}
        self.directory = directory

        self.members = []
        self.new_members = []
        self.epoch = None
        self.conversation = None

    def __getitem__(self, cid):
        return self.state[cid]

    def removal_key(self):
        return os.path.join(self.directory, "removal.key")

    def temp(self):
        return tempfile.NamedTemporaryFile(dir=self.directory, delete=False)

    def create_client(self, user):
        user = obj_qid(user)
        ctx = self.ctxs[user['domain']]

        # create wire client
        client = util.create_client(ctx, user, self.prekeys)
        cid = ClientIdentity(obj_qid(user), client)

        # setup client state
        directory = os.path.join(self.directory, str(cid))
        self.state[cid] = ClientState(directory)

        # setup mls client
        self.cli(cid, "init", str(cid))

        # upload public key
        pk = base64.b64encode(self.cli(cid, "public-key")).decode('ascii')
        ctx.request('PUT', ctx.mkurl('brig', f'/clients/{client}'),
                    headers=api.std_headers(cid.user),
                    json={'mls_public_keys': {'ed25519': pk}}) \
                .check(status=200)

        return cid

    def generate_key_package(self, cid):
        return self.cli(cid, "key-package", "create")

    def upload_new_key_package(self, cid):
        ctx = self.ctxs[cid.user['domain']]
        kp = self.generate_key_package(cid)
        ctx.upload_key_packages(cid.user, cid.client, [kp]).check(status=201)

    def claim_key_packages(self, cid, user):
        ctx = self.ctxs[cid.user['domain']]
        r = ctx.claim_key_packages(cid.user, user).check(status=200).json()
        for e in r['key_packages']:
            cid1 = ClientIdentity(qid(e['domain'], e['user']), e['client'])
            yield cid1, base64.b64decode(e['key_package'])

    def setup_group(self, cid):
        ctx = self.ctxs[cid.user['domain']]
        conv = ctx.create_conversation(cid.user,
                                       protocol='mls',
                                       creator_client=cid.client) \
                    .check(status=201).json()
        assert conv['protocol'] == 'mls'
        group_id = conv['group_id']
        group = json.loads(self.cli(cid, "group", "create", group_id))
        self[cid].group = group

        self.epoch = 0
        self.conversation = obj_qid(conv)
        self.members = [cid]

    def key_package_file(self, kp):
        with self.temp() as kpf:
            kpf.write(kp)
            return kpf.name

    def create_add_commit(self, cid, users=None, extra_key_packages=None):
        # note: these files are saved in the sender's client dir
        welcome_file = self.temp()
        pgs_file = self.temp()

        key_packages = {}

        for user in (users or []):
            for c, kp in self.claim_key_packages(cid, user):
                key_packages[c] = self.key_package_file(kp)
        if extra_key_packages is not None:
            for c, kp in extra_key_packages.items():
                key_packages[c] = self.key_package_file(kp)

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

    def leave(self, user):
        user = obj_qid(user)
        ctx = self.ctxs[user['domain']]
        ctx.remove_member(user, self.conversation, user).check(status=200)

        # remove states, members and new_members
        self.state = {m:s for m, s in self.state.items() if m.user != user}
        self.members = [m for m in self.members if m.user != user]
        self.new_members = [m for m in self.new_members if m.user != user]

    def send_and_consume_message(self, mp):
        ctx = self.ctxs[mp.sender.user['domain']]
        events = ctx.mls_message(mp.sender.user, mp.message) \
                    .check(status=201).json()
        self.consume_message(mp)
        if mp.welcome:
            ctx.mls_welcome(mp.sender.user, mp.welcome).check(status=201)
            self.consume_welcome(mp.welcome)
        return events

    def send_and_consume_commit(self, mp):
        self.send_and_consume_message(mp)
        self.epoch += 1

    def consume_message(self, mp):
        for cid in self.members:
            if cid == mp.sender: continue
            self.consume_message_for(cid, mp.message)

    def consume_message_for(self, cid, message):
        self.cli(cid, "consume", "--group", "<group-in>", "--group-out", "<group-out>",
                 "--signer-key", self.removal_key(), "-", stdin=message)

    def consume_welcome(self, welcome):
        for cid in self.new_members:
            state = self[cid]
            assert state.group is None
            self.cli(cid,
                "group", "from-welcome", "--group-out", "<group-out>", "-",
                stdin=welcome)

        self.members.extend(self.new_members)
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
