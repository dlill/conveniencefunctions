
#' Extract pars, fixed and parnames from grids
#'
#' @param est.vec 
#' @param est.grid needs condition and ID
#' @param fixed.grid 
#' @param ID 
#'
#' @return
#' @export
#'
#' @examples
cf_make_pars <- function(est.vec, est.grid, fixed.grid, ID){
  if ("dummy" %in% names(est.vec))
    stop("'dummy' should not appear in est.vec (parameter vector passed to objective function)\n")
  est.vec <- c(est.vec, dummy = 1)
  parnames  <- unlist(est.grid[est.grid$ID == ID, setdiff(names(est.grid), c("ID", "condition"))])
  pars <- setNames(est.vec[parnames], names(parnames))
  fixed <- fixed.grid[fixed.grid$ID == ID, setdiff(names(fixed.grid), c("ID", "condition"))]
  parnames <- parnames[parnames != "dummy"]
  return(list(pars = pars, fixed = fixed, parnames = parnames))
}



#' Title
#'
#' @param prd0 
#' @param est.grid 
#' @param fixed.grid 
#'
#' @return
#' @export
#'
#' @examples
cf_PRD_indiv <- function(prd0, est.grid, fixed.grid) {
  
  prd <- function(times, pars, fixed = NULL, deriv = FALSE, conditions = est.grid$condition) {
    if (!is.null(fixed))
      stop("fixed cannot be considered at the moment")
    lapply(setNames(nm = conditions), function(cn) {
      ID <- est.grid$ID[est.grid$condition == cn]
      dummy <- cf_make_pars(pars, est.grid, fixed.grid, ID)
      pars_ <- dummy$pars
      fixed_ <- dummy$fixed
      pred0 <- prd0(times, pars_, fixed = fixed_, deriv = deriv, condtions = conditions)[[1]]
    })
  }
  class(prd) <- c("prdfn", "fn")
  prd
}




#' Fast normL2 
#'
#' @param data 
#' @param prd0 
#' @param errmodel 
#' @param est.grid 
#' @param fixed.grid 
#' @param times 
#' @param attr.name 
#'
#' @return objective function
#' @export
#'
#' @importFrom parallel mclapply
cf_normL2_indiv <- function (data, prd0, errmodel = NULL, est.grid, fixed.grid, times = NULL, attr.name = "data", 
                             FLAGverbose = FALSE, FLAGbrowser = FALSE) {
  timesD <- sort(unique(c(0, do.call(c, lapply(data, function(d) d$time)))))
  if (!is.null(times)) 
    timesD <- sort(union(times, timesD))
  x.conditions <- est.grid$condition
  data.conditions <- names(data)
  e.conditions <- names(attr(errmodel, "mappings"))
  controls <- list(times = timesD, attr.name = attr.name, conditions = intersect(x.conditions, 
                                                                                 data.conditions))
  force(errmodel)
  myfn <- function(..., fixed = NULL, deriv = TRUE, conditions = controls$conditions, simcores = 1) {
    arglist <- list(...)
    arglist <- arglist[match.fnargs(arglist, "pars")]
    
    pouter <- arglist[[1]]
    pars <- c(pouter, fixed)
    
    calc_objval <- function(cn) {
      
      if (FLAGbrowser)
        browser()
      
      ID <- est.grid$ID[est.grid$condition == cn]
      if (FLAGverbose)
        print(ID)
      dummy <- cf_make_pars(pars, est.grid, fixed.grid, ID)
      pars_ <- dummy$pars
      fixed_ <- dummy$fixed
      
      timesD <- controls$times
      attr.name <- controls$attr.name
      
      prediction <- prd0(times = timesD, pars = pars_, fixed = fixed_, deriv = deriv)
      
      err <- NULL
      if (any(is.na(data[[cn]]$sigma))) {
        err <- errmodel(out = prediction[[1]], pars = getParameters(prediction[[1]]), conditions = cn)
        mywrss <- nll(res(data[[cn]], prediction[[1]], err[[1]]))
      } else {
        mywrss <- wrss(res(data[[cn]], prediction[[1]]))
      }
      if (deriv) {
        mywrss$gradient <- mywrss$gradient[names(dummy$parnames)]
        names(mywrss$gradient) <- unname(dummy$parnames)
        
        mywrss$hessian <- mywrss$hessian[names(dummy$parnames),names(dummy$parnames)]
        dimnames(mywrss$hessian) <- list(unname(dummy$parnames), unname(dummy$parnames))
      }
      return(mywrss)
    }
    
    if (simcores == 1)
      objlists <- lapply(setNames(nm = conditions), calc_objval)
    if (simcores > 1)
      objlists <- parallel::mclapply(setNames(nm = conditions), calc_objval, mc.cores = simcores)
    
    out <- objlist(value = 0, 
                        gradient = structure(rep(0, length(names(pars))), names = names(pars)), 
                        hessian = matrix(0, nrow = length(names(pars)), ncol = length(names(pars)), dimnames = list(names(pars), names(pars))))
    out$value <- do.call(sum, lapply(objlists, function(.x) .x$value))
    if (deriv) {
      for (gr in lapply(objlists, function(.x) .x$gradient))
        out$gradient[names(gr)] <- out$gradient[names(gr)] + gr
      for (hs in lapply(objlists, function(.x) .x$hessian))
        out$hessian[rownames(hs), colnames(hs)] <- out$hessian[rownames(hs), colnames(hs)] + hs
      }
    
    # consider fixed: return only derivs wrt pouter
    out$gradient <- out$gradient[names(pouter)]
    out$hessian <- out$hessian[names(pouter), names(pouter)]
    
    attr(out, controls$attr.name) <- out$value
    attr(out, "condition_obj") <- vapply(objlists, function(.x) .x$value, 1)
    return(out)
  }

  class(myfn) <- c("objfn", "fn")
  attr(myfn, "conditions") <- data.conditions
  attr(myfn, "parameters") <- attr(prd0, "parameters")
  attr(myfn, "modelname") <- modelname(prd0, errmodel)
  return(myfn)
}



#' DatapointL2 without the env bullshit
#'
#' @param name 
#' @param time 
#' @param value 
#' @param sigma 
#' @param attr.name 
#' @param condition 
#' @param prd_indiv 
#'
#' @return
#' @export
#'
#' @examples
cf_datapointL2 <- function (name, time, value, sigma = 1, attr.name = "validation", 
          condition, prd_indiv) {
  controls <- list(mu = structure(name, names = value)[1], 
                   time = time[1], sigma = sigma[1], attr.name = attr.name)
  
  myfn <- function(..., fixed = NULL, deriv = TRUE, conditions = NULL, 
                   env = NULL) {
    mu <- controls$mu
    time <- controls$time
    sigma <- controls$sigma
    attr.name <- controls$attr.name
    arglist <- list(...)
    arglist <- arglist[match.fnargs(arglist, c("times", 
                                               "pars"))]
    times <- arglist[[1]]
    times <- sort(c(unique(times, time)))
    pouter <- arglist[[2]]
    prediction <- prd_indiv(times, pouter, condition = condition)
    if (!is.null(conditions) && !condition %in% conditions) 
      return()
    
    if (is.null(conditions) && !condition %in% names(prediction)) 
      stop("datapointL2 requests unavailable condition. Call the objective function explicitly stating the conditions argument.")
    
    datapar <- setdiff(names(mu), names(fixed))
    parapar <- setdiff(names(pouter), c(datapar, names(fixed)))
    time.index <- which(prediction[[condition]][, "time"] == time)
    if (length(time.index) == 0) 
      stop("datapointL2() requests time point for which no prediction is available. Please add missing time point by the times argument in normL2()")
    withDeriv <- !is.null(attr(prediction[[condition]], 
                               "deriv"))
    pred <- prediction[[condition]][time.index, ]
    deriv <- NULL
    if (withDeriv) 
      deriv <- attr(prediction[[condition]], "deriv")[time.index, 
                                                      ]
    pred <- pred[mu]
    if (withDeriv) {
      mu.para <- intersect(paste(mu, parapar, sep = "."), 
                           names(deriv))
      deriv <- deriv[mu.para]
    }
    res <- as.numeric(pred - c(fixed, pouter)[names(mu)])
    val <- as.numeric((res/sigma)^2)
    gr <- NULL
    hs <- NULL
    if (withDeriv) {
      dres.dp <- structure(rep(0, length(pouter)), names = names(pouter))
      if (length(parapar) > 0) 
        dres.dp[parapar] <- as.numeric(deriv)
      if (length(datapar) > 0) 
        dres.dp[datapar] <- -1
      gr <- 2 * res * dres.dp/sigma^2
      hs <- 2 * outer(dres.dp, dres.dp, "*")/sigma^2
      colnames(hs) <- rownames(hs) <- names(pouter)
    }
    out <- objlist(value = val, gradient = gr, hessian = hs)
    attr(out, controls$attr.name) <- out$value
    attr(out, "prediction") <- pred
    attr(out, "env") <- env
    return(out)
  }
  class(myfn) <- c("objfn", "fn")
  attr(myfn, "conditions") <- condition
  attr(myfn, "parameters") <- value[1]
  return(myfn)
}