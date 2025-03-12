# Web app settings

## Enforce desktop application only

Wire desktop app is based on Electron and renders Wire web app in a chromium-based wrapper. At the same time Wire web app may be loaded in standard browsers like for example Chrome as well. However, this may not be desirable as under certain circumstances standard browsers may not considered a safe environment for running the Wire web app.

When this flag is set to true it will prevent the web app from running in a standard browser and require the Wire desktop app for running Wire web app.

To enforce desktop application only add the following to your configuration of the `webapp` chart:

```yaml
# ...
envVars:
  # ...
  FEATURE_ENABLE_ENFORCE_DESKTOP_APPLICATION_ONLY: "true"
```

## Enforce constant bit rate

By default Wire users can choose, whether to use constant bit rate (CBR) or variable bit rate (VBR) encoding for 1:1 calls (conference calls always use CBR).
Since there is a theoretical risk of information leakage through packet size analysis when using Opus with variable bitrate encoding during audio calls, CBR can be fully enforced for 1:1 calls in the web app, too.

To enforce CBR add the following to your config:

```yaml
envVars:
  # ...
  FEATURE_ENFORCE_CONSTANT_BITRATE: "true"
```

## Disable media plugins

Wire is built for media plugins to be active in the chat windows so that users don’t have to click the link and leave the app. In some cases it may be desired that these plugins get disabled by default. With this setting all media plugins, including but not limited to YouTube, Spotify, Soundcloud, and Vimeo can be disabled.

To disable media plugins add the following to your configuration:

```yaml
# ...
envVars:
  # ...
  FEATURE_ENABLE_MEDIA_EMBEDS: "false"
```

## Enable extra entropy (only on Windows)

The Wire desktop application uses system-dependent source of random bits as an internal entropy source when generating cryptographic keys. In certain cases it may be desired to enable externally generated entropy derived from mouse movement. This option only affects Windows users.

To enable additional entropy during client creation add the following to your configuration:

```yaml
# ...
envVars:
  # ...
  FEATURE_ENABLE_EXTRA_CLIENT_ENTROPY: "true"
```
