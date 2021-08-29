How to connect the desktop application to a custom backend
==========================================================

Introduction
------------

This page explains how to connect the Wire desktop client to a custom Backend.

Prerequisites
--------------

Install Wire either from the App Store, or download it from our website at (https://wire.com/en/download/)

Have a running Wire backend in your infrastructure/cloud. 

Note down the full URL of the webapp served by that backend (e.g. https://app.custom-wire.com )

Windows
-------

- Create a shortcut to the Wire application
- Edit the shortcut ( Right click > Properties )
- Add the following command line parameters to the shortcut: `--env {URL}`, where `{URL}` is the URL of your webapp as noted down above

MacOS
-----

To create the application

- Open Automator 
- Click New application
- Add the "Run shell script" phase
- Type in the script panel the following command: `open -b com.wearezeta.zclient.mac --args --env {URL}`, where `{URL}` is the URL of your webapp as noted down above
- Save the application from Automator (e.g. on your desktop or in Application)
- To run the application: Just open the application you created in the first step

Linux
-----

- Open a Terminal
- Start the application with the command line arguments: `--env {URL}`, where `{URL}` is the URL of your webapp as noted down above

