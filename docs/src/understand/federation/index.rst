.. _federation-understand:

+++++++++++++++++
Wire federation
+++++++++++++++++

Wire Federation, once implemented, aims to allow multiple Wire-server :ref:`backends <glossary_backend>` to federate with each other. That means that a user 1 registered on backend A and a user 2 registered on backend B should be able to interact with each other as if they belonged to the same backend.

.. note::
   Federation is as of January 2022 still work in progress, since the implementation of federation is ongoing, and certain design decision are still subject to change. Where possible documentation will indicate the state of implementation.

   Some sections of the documentation are still incomplete (indicated with a 'TODO' comment). Check back later for updates.

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
