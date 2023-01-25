# Using a Deep Link to connect an App to a Custom Backend

## Introduction

Once you have your own wire-server set up and configured, you may want to use a client other than the web interface (webapp). There are a few ways to accomplish this:

- **Using a Deep Link** (which this page is all about)
- Registering your backend instance with the hosted SaaS backend for re-direction. For which you might need to talk to the folks @ Wire (the company).

Assumptions:

- You have wire-server installed and working
- You have a familiarity with JSON files
- You can place a JSON file on an HTTPS supporting web server somewhere your users can reach.

Supported client apps:

- iOS
- Android

```{note}
Wire deeplinks can be used to redirect a mobile (Android, iOS) Wire app to a specific backend URL. Deeplinks have no further ability implemented at this stage.
```

## Connecting to a custom backend utilizing a Deep Link

A deep link is a special link a user can click on after installing wire, but before setting it up. This link instructs their wire client to connect to your wire-server, rather than wire.com.

### With Added Proxy

In addition to connect to a custom backend a user can specify a socks proxy to add another layer to the network and make the api calls go through the proxy.

## From a user's perspective:

1. First, a user installs the app from the store
2. The user clicks on a deep link, which is formatted similar to: `wire://access/?config=https://eu-north2.mycustomdomain.de/configs/backend1.json` (notice the protocol prefix: `wire://`)
3. The app will ask the user to confirm that they want to connect to a custom backend. If the user cancels, the app exits.
4. Assuming the user did not cancel, the app will download the file `eu-north2.mycustomdomain.de/configs/backend1.json` via HTTPS. If it can't download the file or the file doesn't match the expected structure, the wire client will display an error message (*'sInvalid link'*).
5. The app will memorize the various hosts (REST, websocket, team settings, website, support) specified in the JSON and use those when talking to your backend.
6. In the welcome page of the app, a "pill" (header) is shown at the top, to remind the user that they are now on a custom backend. A button "Show more" shows the URL of where the configuration was fetched from.

### With Added Proxy

In addition to the previous points

7. The app will remember the (proxy host, proxy port, if the proxy need authentication)
8. In the login page the user will see new section to add the proxy credentials if the proxy need authentication

## From the administrator's (your) perspective:

You need to host two static files, then let your users know how to connect. There are three options listed (in order of recommendation) for hosting the static files.

Note on the meaning of the URLs used below:

`backendURL`

: Use the backend API entrypoint URL, by convention `https://nginz-https.<domain>`

`backendWSURL`

: Use the backend Websocket API entrypoint URL, by convention `https://nginz-ssl.<domain>`

`teamsURL`

: Use the URL to the team settings part of the webapp, by convention `https://teams.<domain>`

`accountsURL`

: Use the URL to the account pages part of the webapp, by convention `https://account.<domain>`

`blackListURL`

: is used to disable old versions of Wire clients (mobile apps). It's a prefix URL to which e.g. `/ios` or `/android` is appended. Example URL for the wire.com production servers: `https://clientblacklist.wire.com/prod` and example json files: [android](https://clientblacklist.wire.com/prod/android) and [iPhone](https://clientblacklist.wire.com/prod/ios) .

`websiteURL`

: Is used as a basis for a few links within the app pointing to FAQs and troubleshooting pages for end users. You can leave this as `https://wire.com` or host your own alternative pages and point this to your own website with the equivalent pages references from within the app.

`title`

: Arbitrary string that may show up in a few places in the app. Should be used as an identifier of the backend servers in question.

### With Added Proxy

`apiProxy:host (optional)`

: Is used to specify a proxy to be added to the network engine, so the API calls will go through it to add more security layer.

`apiProxy:port (optional)`

: Is used to specify the port number for the proxy when we create the proxy object in the network layer.

`apiProxy:needsAuthentication (optional)`

: Is used to specify if the proxy need an authentication, so we can show the section during the login to enter the proxy credentials.

#### Host a deeplink together with your Wire installation

As of release `2.117.0` from `2021-10-29` (see `release notes<release-notes>`), you can configure your deeplink endpoints to match your installation and DNS records (see explanations above)

```yaml
# override values for wire-server
# (e.g. under ./values/wire-server/values.yaml)
nginz:
  nginx_conf:
    deeplink:
      endpoints:
        backendURL: "https://nginz-https.example.com"
        backendWSURL: "https://nginz-ssl.example.com"
        teamsURL: "https://teams.example.com"
        accountsURL: "https://account.example.com"
        blackListURL: "https://clientblacklist.wire.com/prod"
        websiteURL: "https://wire.com"
      apiProxy: # (optional)
        host: "socks5.proxy.com"
        port: 1080
        needsAuthentication: true
      title: "My Custom Wire Backend"
```

(As with any configuration changes, you need to apply them following your usual way of updating configuration (e.g. 'helm upgrade...'))

Now both static files should become accessible at the backend domain under `/deeplink.json` and `deeplink.html`:

- `https://nginz-https.<domain>/deeplink.json`
- `https://nginz-https.<domain>/deeplink.html`

#### Host a deeplink using minio (deprecated)

*If possible, prefer the option in the subsection above or below. This subsection is kept for backwards compatibility.*

**If you're using minio** installed using the ansible code from [wire-server-deploy](https://github.com/wireapp/wire-server-deploy/blob/master/ansible/), then the [minio ansible playbook](https://github.com/wireapp/wire-server-deploy/blob/master/ansible/minio.yml#L75-L88) (make sure to override these variables) creates a json and a html file in the right format, and makes it accessible at `https://assets.<domain>/public/deeplink.json` and at `https://assets.<domain>/public/deeplink.html`

#### Host a deeplink file using your own web server

Otherwise you need to create a `.json` file, and host it somewhere users can get to. This `.json` file needs to specify the URLs of your backend. For the production wire server that we host, the JSON would look like:

```json
{
   "endpoints" : {
      "backendURL" : "https://prod-nginz-https.wire.com",
      "backendWSURL" : "https://prod-nginz-ssl.wire.com",
      "blackListURL" : "https://clientblacklist.wire.com/prod",
      "teamsURL" : "https://teams.wire.com",
      "accountsURL" : "https://accounts.wire.com",
      "websiteURL" : "https://wire.com"
   },
   "apiProxy" : {
      "host" : "socks5.proxy.com",
      "port" : 1080,
      "needsAuthentication" : true
   },
   "title" : "Production"
}
```

**IMPORTANT NOTE:** Clients require **ALL** keys to be present in the JSON file; if some of these keys are irrelevant to your installation (e.g., you don't have a websiteURL) you can leave these values as indicated in the above example.

There is no requirement for these hosts to be consistent, e.g. the REST endpoint could be `wireapp.pineapple.com` and the team setting `teams.banana.com`. If you have been following this documentation closely, these hosts will likely be consistent in naming, regardless.

You now need to get a link referring to that `.json` file to your users, prepended with `wire://access/?config=`. For example, you can save the above `.json` file as `https://example.com/wire.json`, and save the following HTML content as `https://example.com/wire.html`:

```html
<html>
  <head></head>
  <body>
    <a href="wire://access/?config=https://example.com/wire.json">link</a>
  </body>
</html>
```

## Next steps

Now, you can e.g. email or otherwise provide a link to the deeplink HTML page to your users on their mobile devices, and they can follow the above procedure, by clicking on `link`.
