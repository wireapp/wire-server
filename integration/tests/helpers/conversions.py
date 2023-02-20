import dataclasses

class QID:
    def __init__(self, domain, id):
        self.domain = domain
        self.id = id

    def path(self):
        return self.domain + '/' + self.id

    @classmethod
    def from_obj(cls, obj):
        if 'qualified_id' in obj:
            return cls(**obj['qualified_id'])
        else:
            return cls(**obj)

    def __hash__(self):
        return hash((self.domain, self.id))

    def __eq__(self, other):
        return (self.domain, self.id) == (other.domain, other.id)

    def __ne__(self, other):
        return not self == other

    def dict(self):
        return {'domain': self.domain, 'id': self.id}

def conv_canonical(obj):
    obj = dict(obj)
    obj['members'] = dict(obj['members'])
    obj['members']['others'] = sorted(obj['members']['others'],
                                      key=lambda o: o['id'])
    return obj

def obj_id(user):
    if isinstance(user, str):
        return user
    else:
        return user['id']

def obj_path(user): return QID.from_obj(user).path()

def conv_v2(conv):
    """Turn conversation to v2 format."""

    conv = dict(conv)
    conv['access_role_v2'] = conv['access_role']
    conv['access_role'] = 'activated'
    return conv
