# Posit deployment

Search Helper is a standard R Shiny application with `app.R` as its entry point.

## Runtime requirement

The deployed application must have the following environment variable available:

```
LENS_API_TOKEN
```

Do not commit the token to this repository or place it in `manifest.json`.

## Posit Connect Cloud

Connect Cloud requires a `manifest.json` describing the R version and package dependencies.

From the repository root in R:

```r
install.packages("rsconnect")
rsconnect::writeManifest()
```

Commit the generated `manifest.json` only after reviewing it and confirming that it contains dependency metadata but no credentials. Connect Cloud can then deploy the repository using that manifest.

## Posit Connect

For Posit Connect, either publish from RStudio or configure `rsconnect` and deploy from R:

```r
install.packages("rsconnect")
rsconnect::deployApp(appDir = ".")
```

For reproducible production deployments, use `renv` to pin package versions, then regenerate the manifest after updating the lockfile.

## Pre-deployment checks

Before publishing:

1. Confirm all GitHub validation tests pass.
2. Run the app locally and complete both entry routes.
3. Confirm `LENS_API_TOKEN` is available in the deployment environment.
4. Generate a fresh `manifest.json`.
5. Confirm the final-search text download and HTML audit download work.
6. Confirm the application can make outbound HTTPS requests to `api.lens.org`.

The application does not write to Lens or any other external service.
