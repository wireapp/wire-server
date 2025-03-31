# How to connect the desktop application to a custom backend

## Introduction

This page explains how to connect the Wire desktop client to a custom Backend, which can be done either via a start-up parameter or via an initialization file.

## Prerequisites

Install Wire either from the App Store, or download it from our website at (<https://wire.com/en/download/>)

Have a running Wire backend in your infrastructure/cloud.

Note down the full URL of the webapp served by that backend (e.g. <https://app.custom-wire.com> )

## Using start-up parameters

### Windows

- Create a shortcut to the Wire application
- Edit the shortcut ( Right click > Properties )
- Add the following command line parameters to the shortcut: `--env {URL}`, where `{URL}` is the URL of your webapp as noted down above

### MacOS

To create the application

- Open Automator
- Click New application
- Add the "Run shell script" phase
- Type in the script panel the following command: `open -b com.wearezeta.zclient.mac --args --env {URL}`, where `{URL}` is the URL of your webapp as noted down above
- Save the application from Automator (e.g. on your desktop or in Application)
- To run the application: Just open the application you created in the first step

### Linux

- Open a Terminal
- Start the application with the command line arguments: `--env {URL}`, where `{URL}` is the URL of your webapp as noted down above

## Using an initialization file

By providing an initialization file the instance connection parameters and/or proxy settings for the Wire desktop application can be pre-configured. This requires Wire version >= 3.27.

Create a file named `init.json` and set `customWebAppURL` and optionally `proxyServerURL` e.g. as follows:

```json
{
  "customWebAppURL": "https://app.custom-wire.com",
  "env": "CUSTOM",
  "proxyServerURL": "http://127.0.0.1:3128",
}
```

The `env` setting must be set to `CUSTOM` for this to work.

```{note}
Consult your site admin to learn what goes into these settings. The value of `customWebAppURL` can be found [here](https://github.com/wireapp/wire-server/blob/e6aa50913cdcfde1200114787baabf7896394a2f/charts/webapp/templates/deployment.yaml#L40-L41) or [resp. here](https://github.com/wireapp/wire-server/blob/e6aa50913cdcfde1200114787baabf7896394a2f/charts/webapp/values.yaml#L26).  The value of `proxyServerURL` is your browser proxy.  It depends on the configuration of the network your client is running in.
```

### Windows

Move the `init.json` file to `%APPDATA%\Wire\config\init.json` if it does not already exist. Otherwise update it accordingly.

### MacOS

Move the `init.json` file to

```
~/Library/Containers/com.wearezeta.zclient.mac/Data/Library/Application\ Support/Wire/config/init.json
```

if it does not already exist. Otherwise, update it accordingly.

### Linux

On Linux the `init.json` file should be located in the following directory:

```
$HOME/.config/Wire/config/init.json
```
