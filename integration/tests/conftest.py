import pytest
from helpers import context
from helpers.prekeys import Prekeys

CTX = context.Context({'brig': 8082, 'galley': 8085, 'cannon': 8083},
                      version=3)

# check that services are up
if not CTX.check_status('brig'):
    print("Brig does not seem to be running. Aborting tests.")
    import sys
    sys.exit(1)

LOCAL_DOMAIN = 'example.com'

@pytest.fixture
def ctx(): return CTX

@pytest.fixture
def prekeys(): return Prekeys.new()

@pytest.fixture
def domain(): return LOCAL_DOMAIN
