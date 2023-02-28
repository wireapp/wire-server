from .conversions import obj_qid

def conversation(conv, *, ctype=0, creator=None, name=None, members=None):
    assert conv['type'] == ctype
    if creator is not None:
        assert conv['creator'] == creator
    assert conv.get('name') == name
    if members is not None:
        assert set(obj_qid(m) for m in conv['members']['others']) == \
            set(obj_qid(u) for u in members)
