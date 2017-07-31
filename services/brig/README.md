## Brig

The main vessel of the server fleet with a dedicated [gundeck](https://github.com/wire-server/services/gundeck/)
carrying the [cannons](https://github.com/wire-server/services/cannon/).

## Table of Contents

* [Overview](#overview)

## Overview

### Verification Codes

    echo -n "hello" | openssl dgst -sha256 -binary | head -c 16 | openssl enc -base64
