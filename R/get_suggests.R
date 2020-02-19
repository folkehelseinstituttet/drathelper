#' get_suggests
#' @param package a
#' @export
get_suggests <- function(package) {
  d <- data.frame(read.dcf(file = system.file("DESCRIPTION", package = package)))$Suggests
  d <- stringr::str_replace_all(d, "\n", " ")
  d <- stringr::str_split(d, ", ")[[1]]
  d
}
