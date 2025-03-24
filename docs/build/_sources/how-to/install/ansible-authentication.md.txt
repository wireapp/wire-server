(ansible-authentication)=

# Manage ansible authentication settings

Ansible works best if

- you use ssh keys, not passwords
- the user you use to ssh is either `root` or can become `root` (can run `sudo su -`) without entering a password

However, other options are possible, see below:

## How to use password authentication when you ssh to a machine with ansible

If, instead of using ssh keys to ssh to a remote machine, you want to use passwords:

```
sudo apt install sshpass
```

- in hosts.ini, uncomment the 'ansible_user = ...' line, and change '...' to the user you want to login as.
- in hosts.ini, uncomment the 'ansible_ssh_pass = ...' line, and change '...' to the password for the user you are logging in as.
- in hosts.ini, uncomment the 'ansible_become_pass = ...' line, and change the ... to the password you'd enter to sudo.

## Configuring SSH keys

(from <https://linoxide.com/how-tos/ssh-login-with-public-key/>) If you
want a bit higher security, you can copy SSH keys between the machine
you are administrating with, and the machines you are managing with
ansible.

- Create an SSH key.

```
ssh-keygen -t rsa
```

- Install your SSH key on each of the machines you are managing with
  ansible, so that you can SSH into them without a password:

```
ssh-copy-id -i ~/.ssh/id_rsa.pub $USERNAME@$IP
```

Replace `$USERNAME` with the username of the account you set up when
you installed the machine.

## Sudo without password

Ansible can be configured to use a password for switching from the
unpriviledged \$USERNAME to the root user. This involves having the
password lying about, so has security problems. If you want ansible to
not be prompted for any administrative command (a different security
problem!):

- As root on each of the nodes, add the following line at the end of
  the /etc/sudoers file:

```
<ANSIBLE_LOGIN_USERNAME>     ALL=(ALL) NOPASSWD:ALL
```

Replace `<ANSIBLE_LOGIN_USERNAME>` with the username of the account
you set up when you installed the machine.
