# miniCRAN::
#   utils::

drat_repo <- "/git/drat"

win_version <- c("3.6")

get_description_src <- function(drat_repo, pkg, version) {
  utils::untar(
    file.path(drat_repo, "src", "contrib", paste0(pkg, "_", version, ".tar.gz")),
    files = file.path(pkg, "DESCRIPTION"),
    exdir = tempdir()
  )
  x <- data.table::data.table(read.dcf(file = file.path(tempdir(), pkg, "DESCRIPTION")))
  unlink(file.path(tempdir(), pkg))
  x
}

get_description_win <- function(drat_repo, pkg, version) {
  utils::unzip(
    file.path(drat_repo, "bin", "windows", "contrib", win_version, paste0(pkg, "_", version, ".zip")),
    files = file.path(pkg, "DESCRIPTION"),
    exdir = tempdir()
  )
  x <- data.table::data.table(read.dcf(file = file.path(tempdir(), pkg, "DESCRIPTION")))
  unlink(file.path(tempdir(), pkg))
  x
}

#' get_package_table
#' @param drat_repo a
#' @param win_version a
#' @export
get_package_table <- function(drat_repo, win_version) {
  pkgs_src <- data.table::data.table(read.dcf(file.path(drat_repo, "src", "contrib", "PACKAGES")))
  pkgs_win <- data.table::data.table(read.dcf(file.path(drat_repo, "bin", "windows", "contrib", win_version, "PACKAGES")))
  pkgs_cran <- data.table::data.table(miniCRAN::getCranDescription(pkgs_src$Package))
  pkgs_cran <- pkgs_cran[pkgs_cran$Package %in% pkgs_src$Package, ]

  res <- vector("list", length = nrow(pkgs_src))
  for (i in seq_along(res)) {
    pkg <- pkgs_src$Package[i]

    desc_src <- get_description_src(
      drat_repo = drat_repo,
      pkg = pkg,
      version = pkgs_src[pkgs_src$Package == pkg, ]$Version
    )

    retval <- data.frame(
      Package = glue::glue("<a href='packages/{desc_src$Package}.html'>{desc_src$Package}</a>"),
      Source = desc_src$Version,
      Windows = NA,
      CRAN = glue::glue(""),
      Title = desc_src$Title
    )

    if (pkg %in% pkgs_win$Package) {
      desc_win <- get_description_src(
        drat_repo = drat_repo,
        pkg = pkg,
        version = pkgs_src[pkgs_src$Package == pkg, ]$Version
      )
      retval$Windows <- desc_win$Version
    }

    if (pkg %in% pkgs_cran$Package) {
      desc_cran <- pkgs_cran[pkgs_cran$Package == pkg, ]
      retval$CRAN <- glue::glue("<a href='https://cran.r-project.org/web/packages/{pkg}/index.html'>{desc_cran$Version}</a>")
    }
    res[[i]] <- retval
  }

  res <- data.table::rbindlist(res)

  ht <- huxtable::hux(res, add_colnames = T)
  ht <- huxtable::theme_article(ht)
  huxtable::escape_contents(ht)[, c(1, 4)] <- FALSE
  ht
}

#' create_website_index
#' @param drat_repo a
#' @param win_version a
#' @param output_dir a
#' @export
create_website_index <- function(drat_repo, win_version, output_dir = drat_repo) {
  rmarkdown::render(
    system.file("extdata", "index.Rmd", package = "drathelper"),
    output_dir = output_dir,
    params = list(
      drat_repo = drat_repo,
      win_version = win_version
    )
  )
}

refer_to_other_packages <- function(val, pkgs_src) {
  if (is.null(val)) {
    return("")
  }
  if (!is.na(val)) {
    val <- stringr::str_replace_all(val, "\\n", " ")
    val <- stringr::str_split(val, ", ")[[1]]

    val_first <- stringr::str_split(val, " ")
    val_first <- lapply(val_first,function(x) x[1])
    val_first <- unlist(val_first)

    val[val_first %in% pkgs_src$Package] <- paste0(
      "<a href='",
      val_first[val_first %in% pkgs_src$Package],
      ".html'>",
      val[val_first %in% pkgs_src$Package],
      "</a>"
    )

    val <- paste0(val, collapse = ", ")
  }
  return(val)
}

#' create_website_packages
#' @param drat_repo a
#' @param win_version a
#' @param output_dir a
#' @param pkgdown_base_url a
#' @export
create_website_packages <- function(drat_repo, win_version, output_dir = file.path(drat_repo, "packages"), pkgdown_base_url) {
  unlink(output_dir)
  dir.create(output_dir, showWarnings = F)

  pkgs_src <- data.table::data.table(read.dcf(file.path(drat_repo, "src", "contrib", "PACKAGES")))
  pkgs_win <- data.table::data.table(read.dcf(file.path(drat_repo, "bin", "windows", "contrib", win_version, "PACKAGES")))
  pkgs_cran <- data.table::data.table(miniCRAN::getCranDescription(pkgs_src$Package))
  pkgs_cran <- pkgs_cran[pkgs_cran$Package %in% pkgs_src$Package, ]

  for (pkg in pkgs_src$Package) {
    desc_src <- get_description_src(
      drat_repo = drat_repo,
      pkg = pkg,
      version = pkgs_src[pkgs_src$Package == pkg, ]$Version
    )
    desc_src$Depends <- refer_to_other_packages(
      val = desc_src$Depends,
      pkgs_src = pkgs_src
    )
    desc_src$Imports <- refer_to_other_packages(
      val = desc_src$Imports,
      pkgs_src = pkgs_src
    )
    desc_src$Suggests <- refer_to_other_packages(
      val = desc_src$Suggests,
      pkgs_src = pkgs_src
    )

    vers_win <- NA
    if (pkg %in% pkgs_win$Package) {
      desc_win <- get_description_src(
        drat_repo = drat_repo,
        pkg = pkg,
        version = pkgs_src[pkgs_src$Package == pkg, ]$Version
      )
      vers_win <- desc_win$Version
    }

    vers_cran <- NA
    if (pkg %in% pkgs_cran$Package) {
      desc_cran <- pkgs_cran[pkgs_cran$Package == pkg, ]
      vers_cran <- glue::glue("<a href='https://cran.r-project.org/web/packages/{pkg}/index.html'>{desc_cran$Version}</a>")
    }

    pkgdown_url <- file.path(pkgdown_base_url, pkg)
    pkgdown_url <- gsub("//$", "/", pkgdown_url)
    if (!httr::http_error(pkgdown_url, followlocation = 0L)) {
      doc <- data.frame("Documentation", pkgdown_url)
    } else {
      doc <- data.frame("Documentation", NA)
    }

    tab <- data.table::rbindlist(list(
      doc,
      data.frame("Version (source)", gsub("\n"," ",desc_src$Version)),
      data.frame("Version (windows)", vers_win),
      data.frame("Version (CRAN)", vers_cran),
      data.frame("Depends", desc_src$Depends),
      data.frame("Imports", desc_src$Imports),
      data.frame("Suggests", desc_src$Suggests),
      data.frame("Author", iconv(desc_src$Author,from="UTF-8",to="latin1")),
      data.frame("Maintainer", desc_src$Maintainer),
      data.frame("License", desc_src$License),
      data.frame("Needs complication", desc_src$NeedsCompilation)
    ), use.names = F)

    rmarkdown::render(
      system.file("extdata", "package.Rmd", package = "drathelper"),
      output_dir = output_dir,
      output_file = paste0(desc_src$Package, ".html"),
      params = list(
        desc_src = desc_src,
        tab = tab
      ),
      envir = new.env()
    )
  }
}
