# SSLyze allowlists forbid extras; also require a completed scan and every expected TLS version.
.server_scan_results | length == 1 and all(.[];
    .connectivity_status == "COMPLETED" and .scan_status == "COMPLETED" and
    ([.scan_result | to_entries[] | select(.key | test("^tls_.*_cipher_suites$")) |
      select(.value.result.accepted_cipher_suites | length > 0) |
      .value.result.tls_version_used] | sort) ==
    ([$policy[0].tls_versions[] | gsub("v|\\."; "_")] | sort))
