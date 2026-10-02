if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Install the rsconnect package first: install.packages('rsconnect')")
}

rsconnect::writeManifest(appDir = ".")
cat("manifest.json created. Review it before committing or deploying.\n")
