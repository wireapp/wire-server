# wire-server 5.39.1

One fix for the nginz chart. Nothing to do before upgrading.

- The team search-visibility endpoint can be called through the public API again: nginz now routes `/teams/{tid}/search-visibility`.

---

## Details

### Fixes
- Route `/teams/{tid}/search-visibility` through nginz (`charts/nginz` default upstream paths): [#5751](https://github.com/wireapp/wire-server/pull/5751), [`5592b8f`](https://github.com/wireapp/wire-server/commit/5592b8f92ff879be325e7caf73fc9d47cd8db84d)

### Configuration
| key | change | required | default |
|---|---|---|---|
| `nginz.nginx_conf.upstreams.galley` | default list gains the path `/teams/([^/]*)/search-visibility$`; only matters if you override the list | no | the chart default list |
