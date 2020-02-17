Troubleshooting during installation
-------------------------------------

Problems with CORS on the webapp
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you have installed wire-server, but the webapp page in your browser has connection problems and throws errors in the console, make sure to check that you have configured the ``CSP_EXTRA_`` environment variables.

In the file that you use as override when running ``helm install/update -f <override values.yaml>``:

.. code:: yaml

   webapp:
      # ... other settings...
      envVars:
        # ... other environment variables ...
        CSP_EXTRA_CONNECT_SRC: "https://*.example.com, wss://*.example.com"
        CSP_EXTRA_IMG_SRC: "https://*.example.com"
        CSP_EXTRA_SCRIPT_SRC: "https://*.example.com"
        CSP_EXTRA_DEFAULT_SRC: "https://*.example.com"
        CSP_EXTRA_FONT_SRC: "https://*.example.com"
        CSP_EXTRA_FRAME_SRC: "https://*.example.com"
        CSP_EXTRA_MANIFEST_SRC: "https://*.example.com"
        CSP_EXTRA_OBJECT_SRC: "https://*.example.com"
        CSP_EXTRA_MEDIA_SRC: "https://*.example.com"
        CSP_EXTRA_PREFETCH_SRC: "https://*.example.com"
        CSP_EXTRA_STYLE_SRC: "https://*.example.com"
        CSP_EXTRA_WORKER_SRC: "https://*.example.com"

See also `example production overrides <https://github.com/wireapp/wire-server-deploy/blob/develop/values/wire-server/prod-values.example.yaml#L193-L204>`__ and `the full list of webapp environment variables <https://github.com/wireapp/wire-web-config-default/blob/master/wire-webapp/.env.defaults>`__

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
