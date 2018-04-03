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

with open(template_version_file) as f:
  new_version = f.readline().replace('\n', '').strip()

try:
  with open(current_version_file) as f:
    current_version = f.readline().replace('\n', '').strip()
except IOError:
  current_version = '0.0.0'

print '-' * 32
print 'New version:    ', new_version
print 'Current version:', current_version
print 'Branch name:    ', pr_branch_name
print 'Status:         ', 'Up to date' if new_version == current_version else 'Fetching...'
print '-' * 32

if new_version != current_version:
  # Clone the wire-emails project
  os.system('git clone https://github.com/wireapp/wire-emails.git')
  os.chdir(emails)

  # Checkout the desired version
  os.system('git checkout %s' % new_version)

  # Move templates to temp
  if os.path.exists(templates):
    shutil.move(templates, temp)

  # Move wire-emails/dist to templates
  shutil.move(dist, templates)
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

  # Move old translations
  for locale in os.listdir(temp):
    new_dir = os.path.join(templates, locale)
    old_dir = os.path.join(temp, locale)
    if not os.path.exists(new_dir):
      shutil.move(old_dir, new_dir)

  # Remove the wire-emails and temp
  shutil.rmtree(emails)
  shutil.rmtree(temp)

  # Commit back to the branch
  # os.chdir(root)
  # os.system('git add .')
  # os.system('git commit -m "Otto fetch emails (%s)"' % new_version)
  # os.system('git checkout -b %s' % pr_branch_name)
  # os.system('git push git@github.com:wireapp/wire-server.git %s' % pr_branch_name)
