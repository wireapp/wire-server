import pytest
from helpers import context
from helpers.prekeys import Prekeys

CTX = context.Context({'brig': 8082, 'galley': 8085},
                      version=3)
LOCAL_DOMAIN = 'example.com'

@pytest.fixture
def ctx(): return CTX

@pytest.fixture
def prekeys(): return Prekeys.new()

@pytest.fixture
def domain(): return LOCAL_DOMAIN
