{{- define "nginz_nginx.conf" }}
user {{ .Values.nginx_conf.user }} {{ .Values.nginx_conf.group }};
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

  map $http_origin $cors_header {
      default "";
      "~^https://([^/]+\.)?{{ .Values.nginx_conf.external_env_domain | replace "." "\\." }}(:[0-9]{2,5})?$" "$http_origin";
  }


  #
  # Rate Limiting
  #

  limit_req_zone $rate_limited_by_zuser zone=reqs_per_user:12m rate=10r/s;
  limit_req_zone $rate_limited_by_addr zone=reqs_per_addr:12m rate=5r/m;

  limit_conn_zone $rate_limited_by_zuser zone=conns_per_user:10m;
  limit_conn_zone $rate_limited_by_addr zone=conns_per_addr:10m;

  # Too Many Requests (420) is returned on throttling
  # TODO: Change to 429 once all clients support this
  limit_req_status 420;
  limit_conn_status 420;

  limit_req_log_level warn;
  limit_conn_log_level warn;

  # Limit by $zauth_user if present and not part of rate limit exemptions
  limit_req zone=reqs_per_user burst=20;
  limit_conn conns_per_user 25;

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

    location /status {
        zauth off;
        access_log off;
        allow 10.0.0.0/8;
        deny all;

        return 200;
    }

    location /vts {
        zauth off;
        access_log off;
        allow 10.0.0.0/8;
        allow 127.0.0.1;
        deny all;

        vhost_traffic_status_display;
        vhost_traffic_status_display_format html;
    }

    # Block "Franz" -- http://meetfranz.com
    if ($http_user_agent ~* Franz) {
        return 403;
    }

    {{ range $path := .Values.nginx_conf.disabled_paths }}
      location {{ $path }} {

        return 404;
      }
    {{ end }}

    #
    # Service Routing
    #

  {{ range $name, $locations := .Values.nginx_conf.upstreams -}}
    {{- range $location := $locations -}}
      {{- if hasKey $location "envs" -}}
        {{- range $env := $location.envs -}}
          {{- if or (eq $env $.Values.nginx_conf.env) (eq $env "all") -}}

            {{- if and (not (eq $.Values.nginx_conf.env "prod")) ($location.doc) -}}
    rewrite ^/api-docs{{ $location.path }}  {{ $location.path }}/api-docs?base_url=https://{{ $.Values.nginx_conf.env }}-nginz-https.{{ $.Values.nginx_conf.external_env_domain }}/ break;
            {{- end }}

    location {{ $location.path }} {

        # remove access_token from logs, see 'Note sanitized_request' above.
        set $sanitized_request $request;
        if ($sanitized_request ~ (.*)access_token=[^&\s]*(.*)) {
            set $sanitized_request $1access_token=****$2;
        }

            {{- if ($location.basic_auth) }}
        auth_basic "Restricted";
        auth_basic_user_file {{ $.Values.nginx_conf.basic_auth_file }};
            {{- end -}}

            {{- if ($location.disable_zauth) }}
        zauth off;

        # If zauth is off, limit by remote address if not part of limit exemptions
              {{- if ($location.unlimited_requests_endpoint) }}
        # Note that this endpoint has no rate limit
              {{- else -}}
        limit_req zone=reqs_per_addr burst=5 nodelay;
        limit_conn conns_per_addr 20;
              {{- end -}}
            {{- end }}

        if ($request_method = 'OPTIONS') {
            add_header 'Access-Control-Allow-Methods' "GET, POST, PUT, DELETE, OPTIONS";
            add_header 'Access-Control-Allow-Headers' "$http_access_control_request_headers, DNT,X-Mx-ReqToken,Keep-Alive,User-Agent,X-Requested-With,If-Modified-Since,Cache-Control,Content-Type";
            add_header 'Content-Type' 'text/plain; charset=UTF-8';
            add_header 'Content-Length' 0;
            return 204;
        }

        proxy_pass         http://{{ $name }};
        proxy_http_version 1.1;

            {{- if ($location.disable_request_buffering) }}
        proxy_request_buffering off;
            {{ end -}}
            {{- if (hasKey $location "body_buffer_size") }}
        client_body_buffer_size {{ $location.body_buffer_size -}};
            {{- end }}
        client_max_body_size {{ $location.max_body_size | default "64k" }};

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
        proxy_set_header   Z-Connection   $zauth_connection;
        proxy_set_header   Z-Provider     $zauth_provider;
        proxy_set_header   Z-Bot          $zauth_bot;
        proxy_set_header   Z-Conversation $zauth_conversation;
        proxy_set_header   Request-Id     $request_id;

            {{- if ($location.allow_credentials) }}
        more_set_headers 'Access-Control-Allow-Credentials: true';
            {{ end -}}

            {{ if ($location.restrict_whitelisted_origin) -}}
        more_set_headers 'Access-Control-Allow-Origin: $cors_header';
            {{- else }}
        more_set_headers 'Access-Control-Allow-Origin: $http_origin';
            {{- end }}

        more_set_headers 'Access-Control-Expose-Headers: Request-Id, Location';
        more_set_headers 'Request-Id: $request_id';
        more_set_headers 'Strict-Transport-Security: max-age=31536000; preload';
    }

          {{- end -}}
        {{- end -}}

      {{- end -}}
    {{- end -}}
  {{- end }}

    {{ if not (eq $.Values.nginx_conf.env "prod")  }}
    #
    # Swagger Resource Listing
    #

    location /api-docs {
        default_type application/json;
        root {{ $.Values.nginx_conf.swagger_root }};
        index resources.json;
        if ($request_method = 'OPTIONS') {
              add_header 'Access-Control-Allow-Methods' "GET, POST, PUT, DELETE, OPTIONS";
              add_header 'Access-Control-Allow-Headers' "$http_access_control_request_headers, DNT,X-Mx-ReqToken,Keep-Alive,User-Agent,X-Requested-With,If-Modified-Since,Cache-Control,Content-Type";
              add_header 'Content-Type' 'text/plain; charset=UTF-8';
              add_header 'Content-Length' 0;
              return 204;
        }
        more_set_headers 'Access-Control-Allow-Origin: $http_origin';
    }
    {{ end }}

    # Swagger UI

    location /swagger-ui {
        zauth  off;
        gzip   off;
        alias /opt/zwagger-ui;
        types {
            application/javascript  js;
            text/css                css;
            text/html               html;
            image/png               png;
        }
    }
  }
}
{{- end }}
