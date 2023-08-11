#!/usr/bin/env python3

import dataclasses
from .frozendict import frozendict


def qid(domain, id):
    return frozendict({"domain": domain, "id": id})


def qid_path(qid):
    return qid["domain"] + "/" + qid["id"]


def obj_qid(obj):
    """
    Extract a qualified ID from an arbitrary dict-like object. If the object
    is already a qualified ID, return the object itself, otherwise assume the ID
    is contained in a `qualified_id` field, and return that.
    """
    if "qualified_id" in obj:
        return obj["qualified_id"]
    else:
        return obj


def obj_path(obj):
    return qid_path(obj_qid(obj))


def conv_canonical(obj):
    obj = dict(obj)
    obj["members"] = dict(obj["members"])
    obj["members"]["others"] = sorted(obj["members"]["others"], key=lambda o: o["id"])
    return obj


def obj_id(user):
    if isinstance(user, str):
        return user
    else:
        return user["id"]


def conv_v2(conv):
    """Turn conversation to v2 format."""

    conv = dict(conv)
    conv["access_role_v2"] = conv["access_role"]
    conv["access_role"] = "activated"
    return frozendict(conv)
