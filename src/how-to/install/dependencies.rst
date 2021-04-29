.. _dependencies:

Dependencies on operator's machine
--------------------------------------------------------------------

.. warning::

    If you already installed Wire by using ``poetry``, please refer to the
    `old version </versions/install-with-poetry/how-to/index.html>`__ of
    the installation guide.

In order to operate a wire-server installation, you'll need a bunch of software
like Ansible, ``kubectl`` and Helm. We provide a way to get all the needed
dependencies for setting up and interacting with a wire-server cluster.

All dependencies for the ``wire-server-deploy`` project are managed using Git submodules,
`Nix <https://nixos.org>`__ and `Direnv <https://direnv.net>`__.
We also provide a pre-built Docker container image with all the dependencies.

Step one is to fetch a release of ``wire-server-deploy`` and make sure all submodules are
updated. This fetches all the required Ansible roles needed to run the wire Ansible playbooks.

::

   git clone --branch master https://github.com/wireapp/wire-server-deploy.git
   cd wire-server-deploy
   git submodule update


Next, there are two ways to get all the required binaries for operating Wire.


(Option 1) Installing dependencies using Direnv and Nix
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

1. `Install Nix <https://nixos.org/download.html>`__
2. `Install Direnv <https://direnv.net/docs/installation.html>`__
3. `Optionally install the Wire cachix cache to download binaries <https://app.cachix.org/cache/wire-server-deploy>`__

Now, enabling ``direnv`` should install all the dependencies and add them to your ``PATH``. Every time you ``cd`` into
the ``wire-server-deploy`` directory, the right dependencies will be available.

::

   direnv allow

   ansible --version
   ansible 2.9.12


(Option 2) Installing dependencies using Docker image
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We provide a Docker container image containing all the dependencies.
On your machine you need to have the ``docker`` binary available. See `how to install docker <https://docker.com>`__.

Then, after downloading your copy of ``wire-server-deploy``, you can run the container when you're in the ``wire-server-deploy``
directory to have all the dependencies and commands available needed for the deployment.

::

   WSD_CONTAINER=quay.io/wire/wire-server-deploy:cdc1c84c1a10a4f5f1b77b51ee5655d0da7f9518

   sudo docker run -it --network=host \
        -v ${SSH_AUTH_SOCK:-nonexistent}:/ssh-agent \
        -v $HOME/.ssh:/root/.ssh \
        -v $PWD:/wire-server-deploy \
        -e SSH_AUTH_SOCK=/ssh-agent \
        $WSD_CONTAINER bash

   # Inside the container
   bash-4.4# ansible --version
   ansible 2.9.12
