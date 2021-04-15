#' predict.prdfn with options bowser and verbose
#'
#' @param times
#' @param pars parframe
#' @param ...
#' @param FLAGverbose
#' @param FLAGbrowser
#' @param prd 
#' @param keep_names 
#' @param FLAGverbose2 
#'
#' @return data.table
#' @export
#'
#' @importFrom data.table rbindlist data.table 
#' @importFrom purrr imap
#' @importFrom dMod as.parvec
#'
#' @examples
cf_predict <- function (prd, times, pars, keep_names = NULL, ncores = 4, FLAGverbose = FALSE, FLAGverbose2 = FALSE, FLAGbrowser = FALSE,deriv = FALSE, ...) {
    if (FLAGverbose2) cat("Simulating", "\n")
  
  out <- parallel::mclapply(X = 1:nrow(pars), mc.cores = ncores, FUN = function(i) {
    if (FLAGverbose) cat("Parameter set", i, "\n")
    if (FLAGbrowser) browser()
    mypar <- dMod::as.parvec(pars, i)
    prediction <- try(prd(times, mypar, deriv = deriv, ...))
    if (inherits(prediction, "try-error")) {
      warning("parameter set ", i, " failed\n")
      return(NULL)
      }
    prediction <- purrr::imap(prediction, function(.x,.y){
      .x <- data.table(.x)
      if (!is.null(keep_names))
        .x[, (setdiff(names(.x), c(keep_names, "time"))) := NULL]
      .x[, `:=`(condition = .y, parframe_rowid = i)]
      .x
      })
    melt(rbindlist(prediction), variable.name = "name", value.name = "value", id.vars = c("time", "condition", "parframe_rowid"))
  })
    if (FLAGverbose2) cat("postprocessing", "\n")
  out <- rbindlist(out[!is.null(out)])

  
  # Make this available as a FLAG
  pars <- cf_parf_getMeta(pars)
  if (!is.null(pars)){
  pars <- data.table(pars)
  pars[, `:=`(parframe_rowid = 1:.N)]
  out <- merge(pars, out, by = "parframe_rowid")
  out$parframe_rowid <- NULL
  }
  out
}






