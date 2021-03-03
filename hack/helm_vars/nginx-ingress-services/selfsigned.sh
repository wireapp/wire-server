
echo '{
    "CN": "integration.example.com",
    "key": {
        "algo": "rsa",
        "size": 2048
    }
}' > csr.json
cfssl gencert -initca csr.json | cfssljson -bare ca

