import pytest
from helpers import context

CTX = context.Context({'brig': 8082})

@pytest.fixture
def ctx(): return CTX
