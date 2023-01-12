(dependencies)=

# Dependencies on operator's machine

In order to operate a wire-server installation, you'll need a bunch of software
like Ansible, `kubectl` and Helm.

Together with a matching checkout of the wire-server-deploy repository,
containing the Ansible Roles and Playbooks, you should be good to go.

Checkout the repository, including its submodules:

```
git clone --branch master https://github.com/wireapp/wire-server-deploy.git
cd wire-server-deploy
git submodule update --init --recursive
```

We provide a container containing all needed tools for setting up and
interacting with a wire-server cluster.

Ensure you have Docker >= 20.10.14 installed, as the glibc version used is
incompatible with older container runtimes.

Your Distro might ship an older version, so best see [how to install docker](https://docker.com).

To bring the tools in scope, we run the container, and mount the local `wire-server-deploy`
checkout into it.

Replace the container image tag with the commit id your `wire-server-deploy`
checkout is pointing to.

```
WSD_COMMIT_ID=cdc1c84c1a10a4f5f1b77b51ee5655d0da7f9518 # set me
WSD_CONTAINER=quay.io/wire/wire-server-deploy:$WSD_COMMIT_ID

sudo docker run -it --network=host \
     -v ${SSH_AUTH_SOCK:-nonexistent}:/ssh-agent \
     -v $HOME/.ssh:/root/.ssh \
     -v $PWD:/wire-server-deploy \
     -e SSH_AUTH_SOCK=/ssh-agent \
     $WSD_CONTAINER bash

# Inside the container
bash-4.4# ansible --version
ansible 2.9.12
```

Once you're in there, you can move on to {ref}`installing kubernetes <ansible-kubernetes>`.

## (Alternative) Installing dependencies using Direnv and Nix

```{warning}
This is an alternative approach to the above "wrapping container" one, which you should only use if you can't get above setup to work.
```

1. [Install Nix](https://nixos.org/download.html)
2. [Install Direnv](https://direnv.net/docs/installation.html)
3. [Optionally install the Wire cachix cache to download binaries](https://app.cachix.org/cache/wire-server)

Now, enabling `direnv` should install all the dependencies and add them to your `PATH`. Every time you `cd` into
the `wire-server-deploy` directory, the right dependencies will be available.

```
direnv allow

ansible --version
ansible 2.9.12
```
