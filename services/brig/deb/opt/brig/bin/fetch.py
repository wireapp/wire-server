#!/usr/bin/python
# coding: utf-8
#
# Wire
# Copyright (C) 2018 Wire Swiss GmbH
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see http://www.gnu.org/licenses/.
#

import os
import shutil
import sys
reload(sys)
sys.setdefaultencoding('utf8')

IGNORE_DIRS = ['billing', 'marketing']

root = os.path.join(os.path.dirname(os.path.realpath(__file__)), os.pardir)
emails = os.path.join(root, 'wire-emails')
templates = os.path.join(root, 'templates')
dist = os.path.join(emails, 'dist')
new_version_file = os.path.join(root, 'new-version')
current_version_file = os.path.join(templates, 'version')
os.chdir(root)

pr_branch_name = os.environ.get('TRAVIS_PULL_REQUEST_BRANCH')

with open(new_version_file) as f:
  new_version = f.readline().replace('\n', '').strip()

try:
  with open(current_version_file) as f:
    current_version = f.readline().replace('\n', '').strip()
except IOError:
  current_version = '0.0.0'

if new_version != current_version and pr_branch_name:
  os.system('git clone https://github.com/wireapp/wire-emails.git')
  os.chdir(emails)
  os.system('git checkout %s' % new_version)
  if os.path.exists(templates):
    shutil.rmtree(templates)
  shutil.move(dist, templates)

  for directory in IGNORE_DIRS:
    if os.path.exists(os.path.join(templates, directory)):
      shutil.rmtree(os.path.join(templates, directory))

  shutil.rmtree(emails)
  shutil.copy(new_version_file, current_version_file)
  os.chdir(root)
  os.system('git add .')
  os.system('git commit -m "Otto build emails"')
  os.system('git checkout -b %s' % pr_branch_name)
  os.system('git push git@github.com:wireapp/wire-server.git %s' % pr_branch_name)
