from .conversions import *

def conversation(conv, *, ctype=0, creator=None, name=None, members=None):
    assert conv['type'] == ctype
    if creator is not None:
        assert conv['creator'] == creator
    assert conv.get('name') == name
    if members is not None:
        assert set(QID.from_obj(m) for m in conv['members']['others']) == \
            set(QID.from_obj(u) for u in members)
