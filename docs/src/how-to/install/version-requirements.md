# Required/Supported versions

*Updated: 26.04.2021*

```{warning}
If you already installed Wire by using `poetry`, which would be the case **only** for ancient (`pre-2020`, version `0.01`) setups, please refer to the
[old version](https://docs.wire.com/versions/install-with-poetry/how-to/index.html) of
the installation guide. If you have never used `poetry` to install Wire, please ignore this note.
```

## Persistence

- Cassandra: 3.11 (OpenJDK 8)
- Elasticsearch: 6.6.0
- Minio
  : - server: latest (tested v2020-03-25)
    - client: latest (tested v2020-03-14)

### Infrastructure

- Ubuntu: 18.04
- Docker: latest
- Kubernetes: 1.19.7

### Automation

- Ansible: 2.9
- Helm: >= v3
