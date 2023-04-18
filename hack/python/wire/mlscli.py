#!/usr/bin/env python3

import os
import subprocess
import random
import json
import string
import tempfile
import shutil
from . import mls_pb2
import pickle
import uuid


def cid2str(client_identity):
    u = client_identity["user"]
    c = client_identity["client"]
    d = client_identity["domain"]
    return f"{u}:{c}@{d}"


def mlscli(state, client_identity, args, stdin=None):
    cdir = state.client_dir

    subst = {}
    if "<group-in>" in args:
        group_in_file = random_path(state)
        with open(group_in_file, "wb") as f:
            f.write(state.group_state)
        subst["<group-in>"] = group_in_file

    want_group_out = "<group-out>" in args
    if want_group_out:
        subst["<group-out>"] = random_path(state)

    args_substd = []
    for arg in args:
        arg_substd = subst.get(arg)
        if arg_substd is not None:
            args_substd.append(arg_substd)
        else:
            args_substd.append(arg)

    all_args = [
        "mls-test-cli",
        "--store",
        os.path.join(cdir, cid2str(state.client_identity), "store"),
    ] + args_substd

    # TODO: maybe add cwd=cdir, not sure if necessary
    p = subprocess.Popen(
        all_args,
        stdout=subprocess.PIPE,
        stdin=subprocess.PIPE if stdin is not None else None,
        stderr=subprocess.PIPE,
    )

    stdout_data, stderr_data = p.communicate(input=stdin if stdin is not None else None)

    if p.returncode != 0:
        msg = f"mls-test-cli failed returned status {p.returncode}\n"
        msg += "=== stdout:\n"
        msg += stdout_data.decode("utf8")
        msg += "=== stderr:\n"
        msg += stderr_data.decode("utf8")
        raise ValueError(msg)

    if want_group_out:
        with open(subst["<group-out>"], "br") as f:
            state.group_state = f.read()

    return stdout_data


def random_letters(n=10):
    return "".join(random.choices(string.ascii_letters, k=n))


def random_path(state):
    return os.path.join(state.client_dir, random_letters())


class ClientState:
    saveable_attrs = {
        "group_state": "binary",
        "client_identity": "json",
        "last_notification": "uuid",
    }

    def __init__(self, client_dir=None):
        if client_dir is None:
            client_dir = tempfile.TemporaryDirectory().name

        object.__setattr__(self, "client_dir", client_dir)
        object.__setattr__(self, "group_state", None)
        object.__setattr__(self, "client_identity", None)
        object.__setattr__(self, "last_notification", None)

        if not os.path.exists(self.client_dir):
            os.makedirs(self.client_dir)

    def _save_attr(self, name, value, typ):
        p = os.path.join(self.client_dir, name)
        if typ == "binary":
            with open(p, "wb") as f:
                f.write(value)
        elif typ == "json":
            with open(p, "w") as f:
                json.dump(value, f)
        elif typ == "uuid":
            with open(p, "w") as f:
                f.write(str(value))
        else:
            raise ValueError(f"Unknown type {type}")

    def _load_attr(self, name, typ):
        p = os.path.join(self.client_dir, name)
        if not os.path.exists(p):
            return
        if typ == "binary":
            with open(p, "rb") as f:
                v = f.read()
                object.__setattr__(self, name, v)
        elif typ == "json":
            with open(p, "r") as f:
                v = json.load(f)
                object.__setattr__(self, name, v)
        elif typ == "uuid":
            with open(p, "r") as f:
                v = uuid.UUID(f.read())
                object.__setattr__(self, name, v)
        else:
            raise ValueError(f"Unknown type {type}")

    def __setattr__(self, name, value):
        typ = ClientState.saveable_attrs.get(name)
        if typ is not None and value is not None:
            self._save_attr(name, value, typ)
        object.__setattr__(self, name, value)

    @staticmethod
    def load(client_dir):
        state = ClientState(client_dir)
        for name, typ in ClientState.saveable_attrs.items():
            state._load_attr(name, typ)
        return state


def key_package_file(state, ref):
    return os.path.join(state.client_dir, cid2str(state.client_identity), ref.hex())


def init_mls_client(state):
    # the arg after 'init' determines will be clientidentity in all keypackages created with the cli thereafter
    mlscli(state, state.client_identity, ["init", cid2str(state.client_identity)])


def get_public_key(state):
    return mlscli(state, state.client_identity, ["public-key"])


def generate_key_package(state):
    kp = mlscli(state, state.client_identity, ["key-package", "create"])
    kp_path = random_path(state)
    with open(kp_path, "wb") as f:
        f.write(kp)
    ref = mlscli(state, state.client_identity, ["key-package", "ref", kp_path])
    dest = key_package_file(state, ref)
    shutil.move(kp_path, dest)

    return kp, ref


def create_group(state, group_id):
    group_state = mlscli(state, state.client_identity, ["group", "create", group_id])
    state.group_state = group_state


def add_member(state, kpfiles):
    # note: these files are saved in the sender's client dir
    welcome_file = os.path.join(state.client_dir, "welcome")
    pgs_file = os.path.join(state.client_dir, "pgs")
    print("pgs_file", pgs_file)

    args = [
        "member",
        "add",
        "--group",
        "<group-in>",
        "--welcome-out",
        welcome_file,
        "--group-state-out",
        pgs_file,
        "--group-out",
        "<group-out>",
    ] + kpfiles

    msg = mlscli(state, state.client_identity, args)

    welcome = b""
    if os.path.exists(welcome_file):
        with open(welcome_file, "rb") as f:
            welcome = f.read()

    with open(pgs_file, "rb") as f:
        pgs = f.read()

    message_package = {
        "sender": state.client_identity,
        "message": msg,
        "welcome": welcome,
        "public_group_state": pgs,
    }
    return message_package


def make_bundle(message_package):
    mp = message_package

    # GroupInfoBundle UnencryptedGroupInfo TreeFull pgsB
    gib = mls_pb2.GroupInfoBundle()
    # GROUP_INFO corresponds to UnencryptedGroupInfo in the haskell types
    gib.group_info_type = mls_pb2.GroupInfoType.GROUP_INFO
    # FULL correseponds to TreeFull in the haskell types
    gib.ratchet_tree_type = mls_pb2.RatchetTreeType.FULL
    gib.group_info = mp["public_group_state"]

    cb = mls_pb2.CommitBundle()
    cb.commit = mp["message"]
    cb.welcome = mp["welcome"]
    cb.group_info_bundle.CopyFrom(gib)

    return cb.SerializeToString()


def consume_welcome(state, welcome):
    args = [
        "group",
        "from-welcome",
        "--group-out",
        "<group-out>",
        "-",
    ]
    mlscli(state, state.client_identity, args, stdin=welcome)


def consume_message(state, msg, removal_key_file, ignore_stale=False):
    args = (
        [
            "consume",
            "--group",
            "<group-in>",
            "--group-out",
            "<group-out>",
            "--signer-key",
            removal_key_file,
        ]
        + (["--ignore-stale"] if ignore_stale else [])
        + ["-"]
    )
    return mlscli(state, state.client_identity, args, stdin=msg)


def create_application_message(state, message_content):
    args = ["message", "--group", "<group-in>", message_content]
    msg = mlscli(state, state.client_identity, args)
    message_package = {
        "sender": state.client_identity,
        "message": msg,
        "welcome": None,
        "public_group_state": None,
    }
    return message_package
