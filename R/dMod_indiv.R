
cf_make_pars <- function(est.vec, est.grid, fixed.grid, ID){
  parnames  <- unlist(est.grid[est.grid$ID == ID, setdiff(names(est.grid), c("ID", "condition"))])
  pars <- setNames(est.vec[parnames], names(parnames))
  fixed <- fixed.grid[fixed.grid$ID == ID, setdiff(names(fixed.grid), c("ID", "condition"))]
  return(list(pars = pars, fixed = fixed, parnames = parnames))
}


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

cf_normL2_indiv <- function (data, prd0, errmodel = NULL, est.grid, fixed.grid, times = NULL, attr.name = "data") {
  timesD <- sort(unique(c(0, do.call(c, lapply(data, function(d) d$time)))))
  if (!is.null(times)) 
    timesD <- sort(union(times, timesD))
  x.conditions <- est.grid$condition
  data.conditions <- names(data)
  e.conditions <- names(attr(errmodel, "mappings"))
  controls <- list(times = timesD, attr.name = attr.name, conditions = intersect(x.conditions, 
                                                                                 data.conditions))
  force(errmodel)
  myfn <- function(..., fixed = NULL, deriv = TRUE, conditions = controls$conditions, 
                   env = NULL) {
    arglist <- list(...)
    arglist <- arglist[match.fnargs(arglist, "pars")]
    
    pouter <- arglist[[1]]
    # pars_out <- colnames(getDerivs(as.parvec(pouter)))
    pars <- c(pouter, fixed)
    pars_out <- names(pars)
    template <- objlist(value = 0, 
                        gradient = structure(rep(0, length(pars_out)), names = pars_out), 
                        hessian = matrix(0, nrow = length(pars_out), ncol = length(pars_out), dimnames = list(pars_out, pars_out)))
    
    for (cn in conditions) {
      ID <- est.grid$ID[est.grid$condition == cn]
      dummy <- cf_make_pars(pars, est.grid, fixed.grid, ID)
      pars_ <- dummy$pars
      fixed_ <- dummy$fixed
      
      timesD <- controls$times
      attr.name <- controls$attr.name
      if (is.null(env)) 
        env <- new.env()
      prediction <- prd0(times = timesD, pars = pars_, fixed = fixed_, deriv = deriv)
      
      err <- NULL
      if ((!is.null(errmodel) & is.null(e.conditions)) | (!is.null(e.conditions) && (cn %in% e.conditions))) {
        err <- errmodel(out = prediction[[1]], pars = getParameters(prediction[[1]]), 
                        conditions = cn)
        mywrss <- nll(res(data[[cn]], prediction[[1]], 
                          err[[cn]]))
      } else {
        mywrss <- wrss(res(data[[cn]], prediction[[1]]))
      }
      
      template$value <- template$value + mywrss$value
      if (deriv) {
        template$gradient[(dummy$parnames)] <- mywrss$gradient[names(dummy$parnames)]
        template$hessian[(dummy$parnames), (dummy$parnames)] <- mywrss$hessian[names(dummy$parnames), names(dummy$parnames)]
      }
    }
    
    template$gradient <- template$gradient[names(pouter)]
    template$hessian <- template$hessian[names(pouter), names(pouter)]
    
    attr(template, controls$attr.name) <- template$value
    if (!is.null(env))
      assign("prediction", prediction, envir = env)
    
    attr(template, "env") <- env
    return(template)
  }

  class(myfn) <- c("objfn", "fn")
  attr(myfn, "conditions") <- data.conditions
  attr(myfn, "parameters") <- attr(x, "parameters")
  attr(myfn, "modelname") <- modelname(x, errmodel)
  return(myfn)
}
