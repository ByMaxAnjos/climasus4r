# Called when the package is attached via library(climasus4r).
# packageStartupMessage() is suppressible with suppressPackageStartupMessages().
.onAttach <- function(libname, pkgname) {
  ver <- tryCatch(
    as.character(utils::packageVersion("climasus4r")),
    error = function(e) "?"
  )
  packageStartupMessage(
    "climasus4r v", ver, " loaded.\n",
    "Run sus_welcome() to explore the integrated health-climate-environment pipeline."
  )
}
