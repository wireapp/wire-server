# How to

This section will cover various administrative tasks that one might need when running a Kubernetes cluster in general (and Wire).

## Test an ingress is working from inside the cluster

List out
Fetch the IP address of your `ingress-nginx-controller-controller` LoadBalancer with:

```
d kubectl get svc ingress-nginx-controller-controller
```

Get the full name of your `fake-aws-sns` pod with:

```
d kubectl get pods
# or
d kubectl get pods -l app=fake-aws-sns
```

Log onto the SNS pod with:

```
d kubectl exec -it fake-aws-sns-xxxxxx-yyyyyyy -- /bin/bash
```

This will drop you into a bash shell. In here, run:

```
curl -k -H "X-Host: WEBAPPDOMAINNAME" -H "Host: WEBAPPDOMAINNAME" https://NGINXCONTROLLERCONTROLLER:443/ -v
```

You should get a valid response and the HTML of webapp if all is working correctly.

## Load an image into containerd in an offline/airgapped environment

If you ever need to load an image in an offline/airgapped environment for an important security update of a Wire or Kubernetes component. First, download the new release you will be installing. For example, with docker:

```
docker pull registry.k8s.io/ingress-nginx/controller:v1.12.1
```

Now save it in `.tar` and replace the forward slashes and colons with underscores, like this:

```
docker save -o registry.k8s.io_ingress-nginx_controller_v1.12.1.tar registry.k8s.io/ingress-nginx/controller:v1.12.1
```

Now copy the `.tar` file over to all of your kubernetes hosts.

Load it into containerd with:

```
ctr -n k8s.io images import registry.k8s.io_ingress-nginx_controller_v1.12.1.tar
```

Verify the image is in the containerd list after import with:

```
ctr -n k8s.io images list | grep ingress-nginx
```

The image is now ready to be used.
