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
