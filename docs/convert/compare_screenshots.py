#!/usr/bin/env sh

import subprocess
import os

output = subprocess.check_output(['find', 'screenshots', '-name', '*_dev.png']).decode('utf8')

for dev in output.splitlines():
   ref = dev.replace('_dev.png', '_ref.png')
   if os.path.exists(dev) and os.path.exists(ref):
      print(dev)
      cmd = ['compare', '-compose', 'src', dev, ref, dev.replace('_dev.png', '_diff.png')]
      print(cmd)
      subprocess.run(cmd)
   else:
      print(f'Cannot compare {dev}')
