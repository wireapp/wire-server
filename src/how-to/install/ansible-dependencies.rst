Dependencies on operator's machine
--------------------------------------------------------------------

.. TODO: simplify this setup, make it work with python 3

You need python2, some python dependencies, a specific version of ansible, and gnu make. Then, you need to download specific ansible roles using ansible-galaxy, and binaries `kubectl` and `helm`. You have two options to achieve this:

(Option 1) How to install the necessary components locally when using Debian or Ubuntu as your operating system
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Install 'poetry' (python dependency management). See also the `poetry documentation <https://poetry.eustace.io/>`__.

This assumes you're using python 2.7 (if you only have python3 available, you may need to find some workarounds):

::

   sudo apt install -y python2.7 python-pip
   curl -sSL https://raw.githubusercontent.com/sdispater/poetry/master/get-poetry.py > get-poetry.py
   python2.7 get-poetry.py --yes
   source $HOME/.poetry/env
   ln -s /usr/bin/python2.7 $HOME/.poetry/bin/python

Install the python dependencies to run ansible.

::

   git clone https://github.com/wireapp/wire-server-deploy.git
   cd wire-server-deploy/ansible
   ## (optional) if you need ca certificates other than the default ones:
   # export CURL_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt
   poetry install

.. note::

    The 'make download-cli-binaries' part of 'make download' requires
    either that you have run this all as root, or that the user you are
    running these scripts can 'sudo' without being prompted for a password.
    I run 'sudo ls', get prompted for a password, THEN run 'make download'.

Download the ansible roles necessary to install databases and kubernetes:

::

   make download


(Option 2) How to use docker on the local host with a docker image that contains all the dependencies
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

On your machine you need to have the `docker` binary available. See `how to install docker <https://docker.com>`__. Then:

::

   docker pull quay.io/wire/networkless-admin

   # cd to a fresh, empty directory and create some sub directories
   cd ...  # you pick a good location!
   mkdir ./admin_work_dir ./dot_kube ./dot_ssh && cd ./admin_work_dir
   # copy ssh key (the easy way, if you want to use your main ssh key pair)
   cp ~/.ssh/id_rsa ../dot_ssh/
   # alternatively: create a key pair exclusively for this installation
   ssh-keygen -t ed25519 -a 100 -f ../dot_ssh/id_ed25519
   ssh-add ../dot_ssh/id_ed25519
   # make sure the server accepts your ssh key for user root
   ssh-copy-id -i ../dot_ssh/id_ed25519.pub root@<server>

   docker run -it --network=host -v $(pwd):/mnt -v $(pwd)/../dot_ssh:/root/.ssh -v $(pwd)/../dot_kube:/root/.kube quay.io/wire/networkless-admin
   # inside the container, copy everything to the mounted host file system:
   cp -a /src/* /mnt
   # and make sure the git repos are up to date:
   cd /mnt/wire-server && git pull
   cd /mnt/wire-server-deploy && git pull
   cd /mnt/wire-server-deploy-networkless && git pull

(The name of the docker image contains ``networkless`` because it was originally constructed for high-security installations without connection to the public internet.  Since then it has grown to be our recommended general-purpose installation platform.)

Now exit the docker container.  On subsequent times:

::

   cd admin_work_dir
   docker run -it --network=host -v $(pwd):/mnt -v $(pwd)/../dot_ssh:/root/.ssh -v $(pwd)/../dot_kube:/root/.kube quay.io/wire/networkless-admin
   cd wire-server-deploy/ansible
   # do work.

Any changes inside the container under the mount-points listed in the
above command will persist (albeit as user ``root``), everything else
will not, so be careful when creating other files.

To connect to a running container for a second shell:

::

   docker exec -it `docker ps -q --filter="ancestor=quay.io/wire/networkless-admin"` /bin/bash
