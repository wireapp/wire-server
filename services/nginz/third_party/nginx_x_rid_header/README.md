OVERVIEW
=======

nginx-x-rid-header is a small module which adds a request-scoped id (uuid) variable that can be used to correlate frontend logging/activity with backend logging/activity.

Currently only supports NGX_LINUX and NGX_DARWIN.

CREDITS
======

Brian Long (<mailto:newobj@gmail.com>, <mailto:brian@dotspots.com>, <http://newobj.net>)

USAGE
=====

1) Add `--add-module=../git/nginx-x-rid-header` to your nginx configure command. On Linux, you should also add `--with-ld-opt=-lossp-uuid` or whatever flavor of uuid-devel comes with your distribution. Now `make` and `make install`.

2) You now have access to a `$request_id` variable. Suggested use:

    log_format main  '$remote_addr - $remote_user [$time_local] "$request" $status $body_bytes_sent "$http_referer" "$http_user_agent" "$http_x_forwarded_for" - $connection $request_time $upstream_cache_status $request_id';
    server {
        listen       80;
        server_name  example.com;
        location / {
            proxy_set_header x-exampledotcom-rid $request_id;
            proxy_pass   http://localhost:8080;
        }
    }

3) On your backend (8080), you can pull the request header `x-exampledotcom-rid`, and log it or tie it to whatever you may like. This makes it really easy to correlate backend exceptions or instrumentation with frontend http request logs.
