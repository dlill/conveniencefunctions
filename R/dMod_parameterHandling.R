# ---------------------------------------------------------- #
# Parframe-class ----
# ---------------------------------------------------------- #

#' Add step column and fitrank
#'
#' @param myparframe a parframe
#' @param tol integer for steps.
#'
#' @return the parframe with columns fitrank and step
#' @export
#' @importFrom purrr map_dbl
add_stepcolumn <- function(myparframe, tol = 1) {
  steps <- dMod:::stepDetect(myparframe$value, tol)
  bla <- 1:nrow(myparframe)
  stepcol <- cumsum(bla%in%steps)
  
  fitrank <- 1:length(stepcol)
  stepsize <- purrr::map_dbl(stepcol, ~sum(stepcol == .x)) 
  mydf <- as.data.frame(myparframe)
  mydf <- mydf[!names(mydf)%in%c("fitrank", "step", "stepsize")]
  mydf <- cbind(fitrank = fitrank, step  = stepcol, stepsize = stepsize, mydf)
  
  return(parframe(mydf, parameters = attr(myparframe, "parameters")))
}

#' get Parameter names of a parframe
#' @param x parframe
#' @export
cf_parf_parNames <- function(x) {
  attr(x, "parameters")
}

#' Title
#'
#' @param pars 
#'
#' @return data.frame of meta columns
#' @export
cf_parf_getMeta <- function(pars){
  pars <- as.data.frame(pars)[cf_parf_metaNames(pars)]
  if (length(pars) == 0)
    return(NULL)
  pars <- cbind(pars, parframe_rowid = 1:nrow(pars))
  dplyr::rename(pars, objvalue = value)
}

#' Title
#'
#' @param pars 
#'
#' @return
#' @export
cf_parf_metaNames <- function(pars){
  setdiff(names(pars), attr(pars, "parameters"))
}


#' @export
cf_parf_metaNames0 <- list(
  mstrust = c("index", "value", "converged", "iterations", "fitrank", "step", "stepsize"),
  other = c("AIC", "BIC",  "valueData", "valueObj"),
  profile = c("constraint", "stepsize", "gamma", "whichPar", "value"),
  l1 = c("value", "converged", "iterations", "lambda")
)



#' Select parameter columns of parframe
#'
#' @param parf parframe
#' @param parameters keine ahnung mehr?
#'
#' @export
cf_parf_getPars <- function(parf) {
  as.data.frame(parf)[attr(pars, "parameters")] 
}



#' Better as_parframe
#' 
#' Adds AIC and BIC automatically, adds stepcolumn automatically
#'
#' @param x 
#' @param sort.by 
#' @param ... 
#'
#' @return
#' @export
#' @md
#' @importFrom data.table rbindlist
cf_as.parframe <- function (x, sort.by = "value", ...) {
  m_stat <- dMod:::stat.parlist(x)
  m_metanames <- c("index", "value", "converged", "iterations")
  m_idx <- which("error" != m_stat)
  m_parframe <- data.frame(index = m_idx, 
                           value = vapply(x[m_idx], function(.x) .x$value, 1), 
                           converged = vapply(x[m_idx], function(.x) .x$converged, TRUE), 
                           iterations = vapply(x[m_idx], function(.x) as.integer(.x$iterations), 1L))
  
  if (!is.null(attr(x[[m_idx[[1]]]], "BIC"))){
    m_parframe <- cbind(m_parframe, 
                        AIC = vapply(x[m_idx], function(.x) attr(.x, "AIC"), 1),
                        BIC = vapply(x[m_idx], function(.x) attr(.x, "BIC"), 1))
    m_metanames <- c(m_metanames, c("AIC", "BIC"))
  }
  
  parameters <- lapply(x[m_idx], function(x) as.data.table(as.list(x$argument)))
  parameters <- data.table::rbindlist(parameters, use.names = TRUE)
  m_parframe <- cbind(m_parframe, parameters)
  
  m_parframe <- m_parframe[order(m_parframe[sort.by]), ]
  
  cf_parframe(m_parframe, parameters = names(x[[m_idx[1]]]$argument), 
              metanames = m_metanames)
}

#' Improved version of parframe
#' 
#' Fits coerces to data.frame and guesses metanames. Adds stepcolumn automatically
#' 
#' @param x 
#' @param parameters 
#' @param metanames 
#' @param obj.attributes 
#' @param tol 
#'
#' @return
#' @export
#'
#' @examples
cf_parframe <- function(x = NULL, parameters = NULL, metanames = NULL, 
                        obj.attributes = NULL, tol = 1) {
  x <- as.data.frame(x) 
  if (is.null(metanames))
    metanames <- intersect(names(x), Reduce(union, cf_parf_metaNames0))
  if (is.null(parameters))
    parameters <- setdiff(names(x),metanames)
  x <- dMod::parframe(x,parameters, metanames, obj.attributes)
  if ("converged" %in% metanames & !"fitrank" %in% metanames)
    x <- add_stepcolumn(x, tol)
  x
}


#' Title
#'
#' @param parf 
#' @param tol 
#'
#' @return
#' @export
#'
#' @examples
cf_parf_getStepRepresentatives <- function(parf, tol = 1) {
  which(as.logical(c(1, diff(parf$step) != 0)))
}


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
  parf0 <- data.frame(parameterSetId = parameterSetId, value = value, as.data.frame(as.list(pars)), index = 0, converged = FALSE)
  cf_parframe(parf0, metanames = c("parameterSetId", "value", "index", "converged"))
}


#' Rbind some parframes
#'
#' @param parflist list(parframes)
#'
#' @return parframe
#' @export
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @importFrom data.table rbindlist
#'
#' @examples
#' parflist <- list(cf_parframe(data.frame(value = 1, a = 1, b = 2), metanames = "value"),
#'                  cf_parframe(data.frame(value = 1, a = 2, parameterSetId = "par2"), metanames = c("value", "parameterSetId")))
#' parflist <- list(cf_parframe(data.frame(value = 1, a = 1), metanames = "value"),
#'                  cf_parframe(data.frame(value = 1, a = 2, parameterSetId = "par2"), metanames = c("value", "parameterSetId")))
#' cf_parf_rbindlist(parflist)
cf_parf_rbindlist <- function(parflist) {
  
  metanames <- lapply(parflist, function(x) attr(x, "metanames"))
  metanames <- do.call(c, metanames)
  metanames <- unique(metanames)
  
  parnames <- lapply(parflist, function(x) attr(x, "parameters")) # could implement check to see that parameter names are the same
  parnames <- do.call(c, parnames)
  parnames <- unique(parnames)
  
  mixedCol <- intersect(metanames, parnames)
  if (length(mixedCol)) stop("The following columns are parameters and metanames: ", paste0(mixedCol, collapse = ", "))
  
  parf <- data.table::rbindlist(parflist, use.names = TRUE, fill = TRUE)
  parf <- cf_parframe(parf, parameters = parnames, metanames = metanames)
  parf
}
