{{- define "nginz_nginx.conf" }}
worker_processes {{ .Values.nginx_conf.worker_processes }};
worker_rlimit_nofile {{ .Values.nginx_conf.worker_rlimit_nofile | default 1024 }};
pid /var/run/nginz.pid;

# nb. start up errors (eg. misconfiguration) may still end up in
# /var/log/nginz/error.log
error_log stderr warn;

events {
  worker_connections {{ .Values.nginx_conf.worker_connections | default 1024 }};
  multi_accept off;
  use epoll;
}

http {
  #
  # Sockets
  #

  sendfile on;
  tcp_nopush on;
  tcp_nodelay on;

  #
  # Timeouts
  #

  client_body_timeout 60;
  client_header_timeout 60;
  keepalive_timeout 75;
  send_timeout 60;

  ignore_invalid_headers off;

  types_hash_max_size 2048;
  map_hash_bucket_size 128;

  variables_hash_bucket_size 256;

  server_names_hash_bucket_size 64;
  server_name_in_redirect off;

  large_client_header_buffers 4 8k;


  #
  # Security
  #

  server_tokens off;

  #
  # Logging
  #
  # Note sanitized_request:
  # We allow passing access_token as query parameter for e.g. websockets
  # However we do not want to log access tokens.
  #

  log_format custom_zeta '$remote_addr $remote_user "$time_local" "$sanitized_request" $status $body_bytes_sent "$http_referer" "$http_user_agent" $http_x_forwarded_for $connection $request_time $upstream_response_time $upstream_cache_status $zauth_user $zauth_connection $request_id $proxy_protocol_addr "$http_tracestate"';
  access_log /dev/stdout custom_zeta;

  #
  # Monitoring
  #
  vhost_traffic_status_zone;

  #
  # Gzip
  #

  gzip on;
  gzip_disable msie6;
  gzip_vary on;
  gzip_proxied any;
  gzip_comp_level 6;
  gzip_buffers 16 8k;
  gzip_http_version 1.1;
  gzip_min_length 1024;
  gzip_types text/plain text/css application/json application/x-javascript text/xml application/xml application/xml+rss text/javascript;

  #
  # This directive ensures that X-Forwarded-For is used
  # as the client's real IP address (since nginz is always
  # behind an ELB, remote_addr now becomes the client's real
  # IP address)
  #

  real_ip_header X-Forwarded-For;
  set_real_ip_from 0.0.0.0/0;

  #
  # Rate Limiting Exemptions
  #

  geo $rate_limit {
      default 1;

  # IPs to exempt can be added in the .Values.nginx_conf.rate_limit and .Values.nginx_conf.simulators helm values
  {{ if (hasKey .Values.nginx_conf "rate_limit_exemptions") }}
    {{ range $ip := .Values.nginx_conf.rate_limit_exemptions }}
        {{ $ip }} 0;
    {{ end }}
  {{ end }}

  {{ if (hasKey .Values.nginx_conf "simulators") }}
    {{ range $ip := .Values.nginx_conf.simulators }}
        {{ $ip }} 0;
    {{ end }}
  {{ end }}
  }

  #
  # Rate Limiting Mapping
  #

  map $rate_limit $rate_limited_by_addr {
      1 "$binary_remote_addr$uri";
      0 "";
  }

  map $rate_limit $rate_limited_by_zuser {
      1 $zauth_user;
      0 "";
  }

  map $rate_limit $rate_limited_by_zuser_path {
      1 "$zauth_user$uri";
      0 "";
  }

  map $http_origin $cors_header {
      default "";
    {{ range $origin := .Values.nginx_conf.allowlisted_origins }}
    {{- range $domain := (prepend
                            $.Values.nginx_conf.additional_external_env_domains
                            $.Values.nginx_conf.external_env_domain)
    -}}
      "https://{{ $origin }}.{{ $domain }}" "$http_origin";
    {{ end }}
    {{ end }}

    # Allow additional origins at random ports. This is useful for testing with an HTTP proxy.
    # It should not be used in production.
    {{ range $origin := .Values.nginx_conf.randomport_allowlisted_origins }}
      "~^https?://{{ $origin }}(:[0-9]{2,5})?$" "$http_origin";
    {{ end }}
    {{/* Allow additional origin FQDNs, if present */}}
    {{- range $origin := .Values.nginx_conf.allowlisted_fqdn_origins }}
      "https://{{ $origin }}" "$http_origin";
    {{- end }}
    {{- if and .Values.nginx_conf.allowlisted_fqdn_origins (not (eq .Values.nginx_conf.env  "staging")) -}}
    {{ fail "allowlisted_fqdn_origins is only cleared for usage in staging."}}
    {{- end }}
  }


  #
  # Rate Limiting
  #

  limit_req_zone $rate_limited_by_zuser zone=reqs_per_user:12m rate={{ .Values.nginx_conf.rate_limit_reqs_per_user }};
  limit_req_zone $rate_limited_by_addr zone=reqs_per_addr:12m rate={{ .Values.nginx_conf.rate_limit_reqs_per_addr }};

{{- range $limit := .Values.nginx_conf.user_rate_limit_request_zones }}
  {{ $limit }}
{{- end }}

  limit_conn_zone $rate_limited_by_zuser zone=conns_per_user:10m;
  limit_conn_zone $rate_limited_by_addr zone=conns_per_addr:10m;

  # Too Many Requests (420) is returned on throttling
  # TODO: Change to 429 once all clients support this
  limit_req_status 420;
  limit_conn_status 420;

  limit_req_log_level warn;
  limit_conn_log_level warn;

  limit_conn conns_per_user 75;

  #
  #  Proxied Upstream Services
  #

  include {{ .Values.nginx_conf.upstream_config }};

  #
  # Mapping for websocket connections
  #

  map $http_upgrade $connection_upgrade {
      websocket upgrade;
      default   '';
  }



  #
  # Locations
  #

  server {
    listen {{ .Values.config.http.httpPort }};
    listen {{ .Values.config.ws.wsPort }}{{ if (.Values.config.ws.useProxyProtocol) }} proxy_protocol{{ end }};

    zauth_keystore {{ .Values.nginx_conf.zauth_keystore }};
    zauth_acl      {{ .Values.nginx_conf.zauth_acl }};
    oauth_pub_key  {{ .Values.nginx_conf.oauth_pub_key }};

    location /status {
        zauth off;
        access_log off;

        return 200;
    }

    # Block "Franz" -- http://meetfranz.com
    if ($http_user_agent ~* Franz) {
        return 403;
    }

    {{ range $path := .Values.nginx_conf.disabled_paths }}
      location ~* ^(/v[0-9]+)?{{ $path }} {

        return 404;
      }
    {{ end }}

    #
    # Service Routing
    #

  {{- $validUpstreams := include "valid_upstreams" . | fromJson }}
  {{ range $name, $locations := $validUpstreams -}}
    {{- range $location := $locations -}}
      {{- if hasKey $location "envs" -}}
        {{- range $env := $location.envs -}}
          {{- if or (eq $env $.Values.nginx_conf.env) (eq $env "all") -}}
            {{- if $location.strip_version }}

    rewrite ^/v[0-9]+({{ $location.path }}) $1;
            {{- end }}

    {{- $versioned := ternary $location.versioned true (hasKey $location "versioned") -}}
    {{- $path := printf "%s%s" (ternary "(/v[0-9]+)?" "" $versioned) $location.path }}

    location ~* ^{{ $path }} {

        # remove access_token from logs, see 'Note sanitized_request' above.
        set $sanitized_request $request;
        if ($sanitized_request ~ (.*)access_token=[^&\s]*(.*)) {
            set $sanitized_request $1access_token=****$2;
        }

            {{- if ($location.basic_auth) }}
        auth_basic "Restricted";
        auth_basic_user_file {{ $.Values.nginx_conf.basic_auth_file }};
            {{- end }}

            {{- if ($location.disable_zauth) }}
        zauth off;

        # If zauth is off, limit by remote address if not part of limit exemptions
              {{- if ($location.unlimited_requests_endpoint) }}
        # Note that this endpoint has no rate limit
              {{- else }}
                {{- if not (hasKey $location "specific_user_rate_limit") }}
        limit_req zone=reqs_per_addr burst=10 nodelay;
        limit_conn conns_per_addr 20;
                {{- end }}
              {{- end }}
            {{- else }}
              {{- if ($location.unlimited_requests_endpoint) }}
                 # Note that this endpoint has no rate limit per user for authenticated requests
              {{- else }}
        limit_req zone=reqs_per_user burst=20 nodelay;
              {{- end }}
            {{- end }}

            {{- if ($location.oauth_scope) }}
        oauth_scope {{ $location.oauth_scope }};
            {{- end }}

            {{- if hasKey $location "specific_user_rate_limit" }}
        limit_req zone={{ $location.specific_user_rate_limit }}{{ if hasKey $location "specific_user_rate_limit_burst" }} burst={{ $location.specific_user_rate_limit_burst }}{{ end }} nodelay;
            {{- end }}

            {{- range $specific_limit := $location.specific_rate_limits }}
        limit_req zone={{ $specific_limit.zone }}{{ if hasKey $specific_limit "burst" }} burst={{ $specific_limit.burst }}{{ end }} nodelay;
            {{- end }}

        if ($request_method = 'OPTIONS') {
            add_header 'Access-Control-Allow-Methods' "GET, POST, PUT, DELETE, OPTIONS";
            add_header 'Access-Control-Allow-Headers' "$http_access_control_request_headers, DNT,X-Mx-ReqToken,Keep-Alive,User-Agent,X-Requested-With,If-Modified-Since,Cache-Control,Content-Type";
            add_header 'Content-Type' 'text/plain; charset=UTF-8';
            add_header 'Content-Length' 0;
            return 204;
        }

        proxy_pass         http://{{ $name }}{{ if hasKey $.Values.nginx_conf.upstream_namespace $name }}.{{ get $.Values.nginx_conf.upstream_namespace $name }}{{end}};
        proxy_http_version 1.1;

            {{- if ($location.disable_request_buffering) }}
        proxy_request_buffering off;
            {{ end -}}
            {{- if (hasKey $location "body_buffer_size") }}
        client_body_buffer_size {{ $location.body_buffer_size -}};
            {{- end }}
        client_max_body_size {{ $location.max_body_size | default $.Values.nginx_conf.default_client_max_body_size }};

            {{ if ($location.use_websockets) }}
        proxy_set_header   Upgrade        $http_upgrade;
        proxy_set_header   Connection     $connection_upgrade;
        proxy_read_timeout 1h;
            {{- else }}
        proxy_set_header   Connection     "";
            {{ end -}}

            {{- if not ($location.disable_zauth) }}
        proxy_set_header   Authorization  "";
            {{- end }}

        proxy_set_header   Z-Type         $zauth_type;
        proxy_set_header   Z-User         $zauth_user;
        proxy_set_header   Z-Client       $zauth_client;
        proxy_set_header   Z-Connection   $zauth_connection;
        proxy_set_header   Z-Provider     $zauth_provider;
        proxy_set_header   Z-Bot          $zauth_bot;
        proxy_set_header   Z-Conversation $zauth_conversation;
        proxy_set_header   Request-Id     $request_id;
        proxy_set_header   Z-Host         $host;

            {{- if ($location.allow_credentials) }}
        more_set_headers 'Access-Control-Allow-Credentials: true';
            {{ end -}}

        more_set_headers 'Access-Control-Allow-Origin: $cors_header';

        more_set_headers 'Access-Control-Expose-Headers: Request-Id, Location, Replay-Nonce';
        more_set_headers 'Request-Id: $request_id';
        more_set_headers 'Strict-Transport-Security: max-age=31536000; preload';
    }

          {{- end -}}
        {{- end -}}

      {{- end -}}
    {{- end -}}
  {{- end }}

    {{- $defaultDomain := index (keys .Values.nginx_conf.deeplink) 0 }}
    {{- if hasKey .Values.nginx_conf "deeplink" }}
    location ~* ^/deeplink.(json|html)$ {
        zauth off;

        # Extract domain from host by removing 'nginz-https.' prefix
        set $domain_file "";
        set $file_ext $1;
        if ($http_host ~ ^nginz-https\.(.+)$) {
            set $domain_file "$1-deeplink.$file_ext";
        }
        
        # Fallback to a default domain if parsing fails
        if ($domain_file = "") {
            set $domain_file "{{ $defaultDomain }}-deeplink.$file_ext";
        }
        
        alias /etc/wire/nginz/conf/$domain_file;
        types {
            application/json  json;
            text/html         html;
        }
        if ($request_method = 'OPTIONS') {
                add_header 'Access-Control-Allow-Methods' "GET, OPTIONS";
                add_header 'Access-Control-Allow-Headers' "$http_access_control_request_headers, DNT,X-Mx-ReqToken,Keep-Alive,User-Agent,X-Requested-With,If-Modified-Since,Cache-Control,Content-Type";
                add_header 'Content-Type' 'text/plain; charset=UTF-8';
                add_header 'Content-Length' 0;
                return 204;
        }
        more_set_headers 'Access-Control-Allow-Origin: $http_origin';
    }
    {{- end }}
  }

  server {
    # even though we don't use zauth for this server block,
    # we need to specify zauth_keystore etc.
    zauth_keystore {{ .Values.nginx_conf.zauth_keystore }};
    zauth_acl      {{ .Values.nginx_conf.zauth_acl }};
    oauth_pub_key  {{ .Values.nginx_conf.oauth_pub_key }};

    listen {{ .Values.config.http.metricsPort }};

    location /vts {
        access_log off;
        zauth off;

        vhost_traffic_status_display;
        vhost_traffic_status_display_format html;
    }
  }

}
{{- end }}
