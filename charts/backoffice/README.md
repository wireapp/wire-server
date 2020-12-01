Backoffice frontend
===================

This chart provides a basic frontend app that is composed of nginx serving swagger and will soon be found here [here](https://github.com/wireapp/wire-server/blob/develop/tools/backoffice-frontend/README.md). It serves as a tool to perform operations on users and teams such as visualising their user profiles, suspending or even deleting accounts. It is used internally at Wire to provide customer support the means to respond to certain queries from our customers and can be used by anyone that decides to deploy it on their cluster(s).

It is intended to be accessed, at the moment, only by means of port forwarding and therefore only available to cluster admins (or more generally, clusters users able to port forward).

:warning: **DO NOT expose this chart to the public internet** with an ingress - doing so would give anyone the ability to read part of the user database, delete users, etc.

Once the chart is installed, and given default values, you can access the frontend with 2 steps:

 * kubectl port-forward svc/backoffice 8080:8080
 * Open your local browser at http://localhost:8080

If you don't directly access your cluster from your machine, you can do the following (note the backoffice requires port 8080 to be used, but that port is already used by the API server of kubernetes, so use another port like 9999 as intermediate step):

* in a terminal from a kubernetes-master node: `kubectl port-forward svc/backoffice 9999:8080`
* from another terminal on your machine: `ssh <kubernetes-master> -L 8080:localhost:9999 -N`
* Access your local browser on http://localhost:8080
