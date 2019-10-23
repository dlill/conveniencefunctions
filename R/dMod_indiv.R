
make_pars <- function(est.vec, est.grid, fixed.grid, ID){
  pars  <- est.vec[est.grid[est.grid$ID == ID, setdiff(names(est.grid, c("ID", "condition")))]]
  fixed <- fixed.grid[fixed.grid$ID == ID, setdiff(names(fixed.grid, c("ID", "condition")))]
  return(list(pars = pars, fixed = fixed))
}


PRD_indiv <- function(prd0, est.grid, fixed.grid) {
  
  prd <- function(times, pars, fixed = NULL, deriv = FALSE, conditions = est.grid$condition) {
    if (!is.null(fixed))
      stop("fixed cannot be considered at the moment")
    lapply(setNames(nm = conditions), function(cn) {
      ID <- est.grid$ID[est.grid$condition == cn]
      dummy <- make_pars(pars, est.grid, fixed.grid, ID)
      pinner <- dummy$pars
      fixedinner <- dummy_fixed
      
      pred0 <- prd0(times, pars, fixed = fixed, deriv = deriv)[[1]]
    })
  }
  
}

normL2_indiv <- function (data, prd0, errmodel = NULL, est.grid, fixed.grid, times = NULL, attr.name = "data") 
{
  timesD <- sort(unique(c(0, do.call(c, lapply(data, function(d) d$time)))))
  if (!is.null(times)) 
    timesD <- sort(union(times, timesD))
  x.conditions <- names(attr(x, "mappings"))
  data.conditions <- names(data)
  if (!all(data.conditions %in% x.conditions)) 
    stop("The prediction function does not provide predictions for all conditions in the data.")
  e.conditions <- names(attr(errmodel, "mappings"))
  controls <- list(times = timesD, attr.name = attr.name, conditions = intersect(x.conditions, 
                                                                                 data.conditions))
  force(errmodel)
  myfn <- function(..., fixed = NULL, deriv = TRUE, conditions = controls$conditions, 
                   env = NULL) {
    arglist <- list(...)
    arglist <- arglist[match.fnargs(arglist, "pars")]
    pouter <- arglist[[1]]
    pars_out <- colnames(getDerivs(as.parvec(pouter)))
    template <- objlist(value = 0, gradient = structure(rep(0, 
                                                            length(pars_out)), names = pars_out), hessian = matrix(0, 
                                                                                                                   nrow = length(pars_out), ncol = length(pars_out), 
                                                                                                                   dimnames = list(pars_out, pars_out)))
    lapply(setNames(nm = conditions), function(cn) {
      ID <- est.grid$ID[est.grid$condition == cn]
      dummy <- make_pars(pouter, est.grid, fixed.grid, ID)
      pouter <- dummy$pars
      fixedinner <- dummy$fixed
    
    timesD <- controls$times
    attr.name <- controls$attr.name
    if (is.null(env)) 
      env <- new.env()
    prediction <- x(times = timesD, pars = pouter, fixed = fixed, 
                    deriv = deriv, conditions = conditions)
    out.data <- lapply(conditions, function(cn) {
      err <- NULL
      if ((!is.null(errmodel) & is.null(e.conditions)) | 
          (!is.null(e.conditions) && (cn %in% e.conditions))) {
        err <- errmodel(out = prediction[[cn]], pars = getParameters(prediction[[cn]]), 
                        conditions = cn)
        mywrss <- nll(res(data[[cn]], prediction[[cn]], 
                          err[[cn]]))
      }
      else {
        mywrss <- wrss(res(data[[cn]], prediction[[cn]]))
      }
      available <- intersect(pars_out, names(mywrss$gradient))
      result <- template
      result$value <- mywrss$value
      if (deriv) {
        result$gradient[available] <- mywrss$gradient[available]
        result$hessian[available, available] <- mywrss$hessian[available, 
                                                               available]
      }
      else {
        result$gradient <- result$hessian <- NULL
      }
      return(result)
    })
    })
    
    out.data <- Reduce("+", out.data)
    out <- out.data
    attr(out, controls$attr.name) <- out.data$value
    if (!is.null(env)) {
      assign("prediction", prediction, envir = env)
    }
    attr(out, "env") <- env
    return(out)
  }
  class(myfn) <- c("objfn", "fn")
  attr(myfn, "conditions") <- data.conditions
  attr(myfn, "parameters") <- attr(x, "parameters")
  attr(myfn, "modelname") <- modelname(x, errmodel)
  return(myfn)
}