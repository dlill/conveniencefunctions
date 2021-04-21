#' Turn a pars vector into a single-row parframe
#'
#' @param obj Objective function like normL2
#' @param pars setNames(outervalues, parnames)
#' @param parameterSetId Identifier for this parameterset
#'
#' @return parframe
#' @export
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#'
#' @examples
#' obj <- function(pars) {list(value = sum(pars^2))}
#' pars = c(a = 1, b = 2)
#' pars2parframe(pars, "base", obj)
pars2parframe <- function(pars, parameterSetId = "Base", obj = NULL) {
  value <- if (!is.null(obj)) obj(pars)$value else NA
  parf0 <- data.frame(parameterSetId = parameterSetId, value = value, as.data.frame(as.list(pars)))
  cf_parframe(parf0, metanames = c("parameterSetId", "value"))
}
