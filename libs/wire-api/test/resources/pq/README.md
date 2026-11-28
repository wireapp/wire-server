# Post-quantum key package fixtures

One basic-credential key package per post-quantum cipher suite (0xF001 to
0xF00B), made by core-crypto, plus the key package ref that core-crypto computed
for it.

- `key_package_0xf0NN.mls`: the TLS-serialised `KeyPackage`
- `key_package_0xf0NN.ref`: the raw `KeyPackageRef` bytes

Generated with core-crypto 6f51a6d450ce8e271bbd35df66e73a4c8777bb29 (branch
`pq-mls-ciphersuites`, openmls rev 770733456132521ff13eb5212efcf56503f25a79).
The client identity is
`b455a431-9db6-4404-86e7-6a3ebe73fcaf:3ae58155@mls.example.com`.
