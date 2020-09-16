#' Title
#'
#' @param dirnm 
#'
#' @return
#' @export
#'
#' @examples
cf_dir.create <- function(dirnm) {
  if (!dir.exists(dirnm)) dir.create(dirnm, recursive = TRUE)
  dirnm
}


#' Title
#'
#' @param dt 
#' @param filename 
#'
#' @return
#' @export
#' 
#' @importFrom knitr kable
#' 
#' @examples
cfoutput_MdTable <- function(dt, filename = NULL, format = c("markdown", "pandoc"), caption = NULL, ...) {
  kt <- knitr::kable(dt,format = format[1], caption = caption, ...)
  if(!is.null(filename)) cfwriteLines(kt, filename)
  cat("# [] implement split table at certain groups")
  kt
}


#' Title
#'
#' @param x 
#' @param filename 
#'
#' @return
#' @export
#'
#' @examples
cfwriteLines <- function(x, filename) {
  cf_dir.create(dirname(filename))
  writeLines(x, filename)
}

#' Title
#'
#' @param object 
#' @param filename 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
cfoutput_RDS <- function(object, filename, ...) {
  cf_dir.create(dirname(filename))
  saveRDS(object, file = filename, ...)
}
