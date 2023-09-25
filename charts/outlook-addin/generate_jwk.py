import json
from jwcrypto import jwk
import base64

def pem_to_jwk(pem_key, is_private=True):
    key = jwk.JWK.from_pem(pem_key)
    if is_private:
        key_dict = key.export(as_dict=True, private_key=True)
    else:
        key_dict = key.export(as_dict=True, private_key=False)
    return key_dict

def convert_to_pem(base64_key):
    pem_key = base64.b64decode(base64_key)
    return pem_key

with open("private_key.pem", "rb") as f:
    private_key_pem = f.read()
    private_key_b64 = base64.b64encode(private_key_pem).decode('utf-8')
    private_jwk = pem_to_jwk(convert_to_pem(private_key_b64), is_private=True)

print("Private JWK:")
print(json.dumps(private_jwk, indent=2))
