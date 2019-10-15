Troubleshooting during installation
-------------------------------------

Problems with ansible and python versions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If for instance the following fails::

    ansible all -i hosts.ini -m shell -a "echo hello"

If your target machine only has python 3 (not python 2.7), you can tell ansible to use python 3 by default, by specifying `ansible_python_interpreter`:

.. code:: ini

   # hosts.ini

   [all]
   server1 ansible_host=1.2.3.4


   [all:vars]
   ansible_python_interpreter=/usr/bin/python3

(python 3 may not be supported by all ansible modules yet)
