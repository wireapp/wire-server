#!/usr/bin/env python3
# coding: utf-8
#
# This file is part of the Wire Server implementation.
#
# Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU Affero General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option) any
# later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
# details.
#
# You should have received a copy of the GNU Affero General Public License along
# with this program. If not, see <https://www.gnu.org/licenses/>.

import os
import shutil
import sys

IGNORE_DIRS = ['billing', 'marketing']
TEAM_SUPPORT = ['en', 'de']
IGNORE_TEAM_DIRS = ['team', 'provider']

root = os.path.join(os.path.dirname(os.path.realpath(__file__)), os.pardir)
emails = os.path.join(root, 'wire-emails')
templates = os.path.join(root, 'templates')
temp = os.path.join(root, 'temp')
css = os.path.join(templates, 'css')
dist = os.path.join(emails, 'dist')
template_version_file = os.path.join(root, 'template-version')
current_version_file = os.path.join(templates, 'version')
os.chdir(root)

pr_branch_name = os.environ.get('TRAVIS_PULL_REQUEST_BRANCH')


# https://stackoverflow.com/a/15824216/8418
def recursive_overwrite(src, dest):
  if os.path.isdir(src):
    if not os.path.isdir(dest):
      os.makedirs(dest)
    files = os.listdir(src)
    for f in files:
      recursive_overwrite(os.path.join(src, f), os.path.join(dest, f))
  else:
    shutil.copyfile(src, dest)

print('')
print('-' * 32)
print('this script clones github.com/wireapp/wire-emails and copies the changes to', dist)
print('to list all versions, `cd wire-emails && git tag -l`.')
print('reading new version from', template_version_file, '...')
print('-' * 32)
print('')

with open(template_version_file) as f:
  new_version = f.readline().replace('\n', '').strip()

try:
  with open(current_version_file) as f:
    current_version = f.readline().replace('\n', '').strip()
except IOError:
  current_version = '0.0.0'

print('-' * 32)
print('New version:    ', new_version)
print('Current version:', current_version)
print('Branch name:    ', pr_branch_name)
print('Status:         ', 'Up to date' if new_version == current_version else 'Fetching...')
print('-' * 32)

if new_version != current_version:
  # Clone the wire-emails project at new version
  os.system('git clone --depth=1 https://github.com/wireapp/wire-emails.git -b %s' % new_version)
  os.chdir(emails)

  # Move wire-emails/dist to templates
  recursive_overwrite(dist, templates)
  if os.path.exists(css):
    shutil.rmtree(css)

  # Deleted unwanted folders
  for root_, subdirs, files in os.walk(templates):
    if root_.split(os.sep)[-1] in IGNORE_DIRS:
      shutil.rmtree(root_)

  # Deleted unwanted team folders
  for root_, subdirs, files in os.walk(templates):
    if root_.split(os.sep)[-2] not in TEAM_SUPPORT and root_.split(os.sep)[-1] in IGNORE_TEAM_DIRS:
      shutil.rmtree(root_)

  # Copy the version number
  shutil.copy(template_version_file, current_version_file)

  # Remove the wire-emails
  shutil.rmtree(emails)

  # Commit back to the branch
  # os.chdir(root)
  # os.system('git add .')
  # os.system('git commit -m "Otto fetch emails (%s)"' % new_version)
  # os.system('git checkout -b %s' % pr_branch_name)
  # os.system('git push git@github.com:wireapp/wire-server.git %s' % pr_branch_name)
