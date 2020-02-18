#' source_to_binary
#' @param packages_to_build a
#' @param drat_repo a
#' @param repos a
#' @export
source_to_binary <- function(
  packages_to_build,
  drat_repo,
  repos = c("https://cran.rstudio.com")
  ) {
  drat::pruneRepo(drat_repo, type = "source", remove = TRUE)
  drat::pruneRepo(drat_repo, type = "win.binary", remove = TRUE)

  pkg_srcs <- list.files(file.path(drat_repo, "src", "contrib"))
  keep <- c()
  for (i in packages_to_build) {
    keep <- c(keep, grep(paste0(i, "_"), pkg_srcs))
  }
  pkg_srcs <- pkg_srcs[keep]
  pkg_with_versions <- gsub(".tar.gz$", "", pkg_srcs)

  for (i in seq_along(pkg_srcs)) {
    devtools::install_local(
      file.path(drat_repo, "src", "contrib", pkg_srcs[i]),
      repos = "https://cran.rstudio.com",
      force = T,
      upgrade = "always"
    )

    pkg_binary <- devtools::build(
      pkg = file.path(drat_repo, "src", "contrib", pkg_srcs[i]),
      path = tempdir(),
      binary = T
    )

    drat::insertPackage(pkg_binary, repodir = drat_repo)
  }
  drat::pruneRepo(drat_repo, type = "win.binary", remove = TRUE)
}
