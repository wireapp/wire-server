+++++++++++++++++
Wire federation
+++++++++++++++++

Wire Federation, once implemented, aims to allow multiple Wire-server :ref:`backends <backend>` to federate with each other.

Federation means that user 1 registered on backend A and user 2 registered on backend B must be able to interact with each other as if they belonged to the same backend.

.. note::
   Federation is as of March 2021 still early work in progress, since the implementation of federation is ongoing, and certain design decision are still subject to change. Where possible documentation will indicate the state of implementation.

..
   comment: The toctree directive below takes a list of the pages you want to appear in order,
   and '*' is used to include any other pages in the federation directory in alphabetical order

.. toctree::
   :maxdepth: 2
   :numbered:
   :glob:

   introduction
   architecture
   *roadmap
   *
