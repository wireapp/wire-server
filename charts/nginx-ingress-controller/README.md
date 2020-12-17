This deploys a single ingress controller - ideally, you want this on a separate, shared namespace since controllers listen on all namespaces by default (you can also modify that but it's generally discouraged).

It is mostly a wrapper of the [nginx-ingress](https://github.com/helm/charts/blob/master/stable/nginx-ingress/README.md) with some other defaults that make sense for our use case(s).

For more options, have a look at [nginx-ingress](https://github.com/helm/charts/blob/master/stable/nginx-ingress/README.md) and other overrides that may be useful for your use case.
