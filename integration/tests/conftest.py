import pytest
from helpers import context
from helpers.prekeys import Prekeys
from helpers.setup.mls import MLS
import tempfile

LOCAL_DOMAIN = 'example.com'
REMOTE_DOMAIN = 'b.example.com'

# TODO: use session-scoped fixtures
CTX = context.Context(LOCAL_DOMAIN,
                      {'brig': 8082, 'galley': 8085, 'cannon': 8083},
                      version=3)
CTX2 = context.Context(REMOTE_DOMAIN,
                       {'brig': 9082, 'galley': 9085, 'cannon': 9083},
                       version=3)

# check that services are up
if not CTX.check_status('brig'):
    print("Brig does not seem to be running. Aborting tests.")
    import sys
    sys.exit(1)

if not CTX2.check_status('brig'):
    CTX2 = None

@pytest.fixture
def ctx(): return CTX

@pytest.fixture
def ctx2():
    if CTX2 is None:
        pytest.skip("no remote domain")
    return CTX2

@pytest.fixture
def prekeys(): return Prekeys.new()

@pytest.fixture
def domain(): return LOCAL_DOMAIN

@pytest.fixture
def mls():
    ctxs = [CTX]
    if CTX2 is not None:
        ctxs.append(CTX2)
    with tempfile.TemporaryDirectory() as directory:
        yield MLS(directory, *ctxs)
