Dependencies on operator's machine
--------------------------------------------------------------------

You need python2, some python dependencies, a specific version of ansible, and gnu make. Then, you need to download specific ansible roles using ansible-galaxy, and binaries `kubectl` and `helm`. You have two options to achieve this:

(Option 1) How to install the necessary components locally when using Debian or Ubuntu as your operating system
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Install 'poetry' (python dependency management). See also the `poetry documentation <https://poetry.eustace.io/>`__.

This assumes you're using python 2.7 (if you only have python3 available, you may need to find some workarounds):

::

   sudo apt install -y python2.7 python-pip
   curl -sSL https://raw.githubusercontent.com/sdispater/poetry/master/get-poetry.py > get-poetry.py
   python2.7 get-poetry.py
   source $HOME/.poetry/env
   ln -s /usr/bin/python2.7 $HOME/.poetry/bin/python

Install the python dependencies to run ansible.

::

   git clone https://github.com/wireapp/wire-server-deploy.git
   cd wire-server-deploy/ansible
   ## (optional) if you need ca certificates other than the default ones:
   # export CURL_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt
   poetry install

Download the ansible roles necessary to install databases and kubernetes

::

   make download


(Option 2) How to use docker on the local host with a docker image that contains all the dependencies
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

On your machine you need to have the `docker` binary available. See `how to install docker <https://docker.com>`__. Then:

::

   docker pull quay.io/wire/networkless-admin

   # cd to a fresh, empty directory and create some sub directories
   mkdir -p wire-installation && cd wire-installation
   mkdir -p ../admin_work_dir && cd ../admin_work_dir
   mkdir -p ../dot_ssh
   mkdir -p ../dot_kube
   # copy ssh key
   cp ~/.ssh/id_rsa ../dot_ssh/

   docker run -it --network=host -v $(pwd):/mnt -v $(pwd)/../dot_ssh:/root/.ssh -v $(pwd)/../dot_kube:/root/.kube quay.io/wire/networkless-admin
   # inside the container:
   cp -a /src/* /mnt
   # run ansible from here. If you make any changes, they will be written to your host file system
   # (those files will be owned by root as docker runs as root)
   cd /mnt/wire-server-deploy/ansible

Any changes inside the container under ``/mnt`` (host system:
``admin_work_dir``) and ``/root/.ssh`` (host system:
``~/.ssh/ssh-for-docker``) will persist (albeit as user ``root``),
everything else will not, so be careful when creating other files.

On subsequent times:

::

   cd admin_work_dir
   docker run -it --network=host -v $(pwd):/mnt -v $(pwd)/../dot_ssh:/root/.ssh -v $(pwd)/../dot_kube:/root/.kube quay.io/wire/networkless-admin
   # do work.

To connect to a running container for a second shell:

::

   docker exec -it `docker ps -q --filter="ancestor=quay.io/wire/networkless-admin"` /bin/bash
