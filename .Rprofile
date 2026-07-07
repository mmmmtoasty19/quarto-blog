source("renv/activate.R")

# Cross-platform binary repo: RSPM auto-detects platform when you drop
# the __linux__/<distro> segment. Falls back to CRAN's own binaries
# on Windows since RSPM's generic endpoint doesn't always cover Windows well.
options(repos = c(
  CRAN = if (Sys.info()[["sysname"]] == "Windows") {
    "https://cloud.r-project.org"
  } else {
    "https://packagemanager.posit.co/cran/__linux__/noble/latest"
  }
))

options(pkgType = "binary")