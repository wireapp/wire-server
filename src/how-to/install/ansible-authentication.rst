Authentication
^^^^^^^^^^^^^^^^^

If, instead of using ssh keys to ssh to a remote machine, you want to use passwords::

   sudo apt install sshpass

* in hosts.ini, uncomment the 'ansible_user = ...' line, and change '...' to the user you want to login as.
* in hosts.ini, uncomment the 'ansible_ssh_pass = ...' line, and change '...' to the password for the user you are logging in as.
* in hosts.ini, uncomment the 'ansible_become_pass = ...' line, and change the ... to the password you'd enter to sudo.

