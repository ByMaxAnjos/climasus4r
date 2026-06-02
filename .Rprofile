# Project-level R profile for climasus4r
#
# Fix: Prevent pak/pkgcache from hitting bioconductor.org to resolve the
# Bioc version for R 4.5.  The env var R_BIOC_CONFIG_URL is read by the
# standalone bioconductor object inside pkgcache (config_url() calls
# Sys.getenv("R_BIOC_CONFIG_URL", "https://bioconductor.org/config.yaml")).
#
# We set it in the *parent* R process; pak's callr subprocess (started with
# user_profile = FALSE) inherits the env var from the parent process and
# uses the local fixture instead of hitting the network.
local({
  bioc_fixture <- tryCatch(
    file.path(find.package("pak"), "library", "pkgcache",
              "fixtures", "bioc-config.yaml"),
    error = function(e) NULL
  )
  if (!is.null(bioc_fixture) && file.exists(bioc_fixture)) {
    Sys.setenv(R_BIOC_CONFIG_URL = paste0("file://", bioc_fixture))
  }
})
