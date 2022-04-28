# Web app settings

## Enforce desktop application only

Wire desktop app is based on Electron and renders Wire web app in a chromium-based wrapper. At the same time Wire web app may be loaded in standard browsers like for example Chrome as well. However, this may not be desirable as under certain circumstances standard browsers may not considered a safe environment for running the Wire web app.

When this flag is set to true it will prevent the web app from running in a standard browser and require the Wire desktop app for running Wire web app.

To enforce desktop application only add the following to your Helm overrides in `values/wire-server/values.yaml`:

```yaml
webapp:
  # ...
  envVars:
    # ...
    FEATURE_ENABLE_ENFORCE_DESKTOP_APPLICATION_ONLY: "true"
```

## Enforce constant bit rate

Since there is a theoretical risk of information leakage through packet size analysis when using Opus with variable bitrate encoding during audio calls, constant bit rate encoding (CBR) can be enforced for all call types (conference and 1:1).


To enforce CBR add the following to your Helm overrides in `values/wire-server/values.yaml`:

```yaml
webapp:
  # ...
  envVars:
    # ...
    FEATURE_ENFORCE_CONSTANT_BITRATE: "true"
```
