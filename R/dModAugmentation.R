# -------------------------------------------------------------------------#
# Profiles ----
# -------------------------------------------------------------------------#


#' getProfiles with optimum below the fit$argument
#' 
#' Sometimes, a profile finds a better optimum
#' Get only profiles of parameters which show this behaviour
#' 
#' @param profiles parframe
#'
#' @return data.table
#' @export
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
cf_profile_getProfilesOptimumBelowFit <- function(profiles){
  dp <- as.data.table(profiles)
  value0 <- dp[constraint == 0, unique(value)]
  parsBelow <- dp[value < value0, unique(whichPar)]
  
  profiles <- profiles[profiles$whichPar %in% parsBelow]
  profiles
}


#' Title
#'
#' @param profiles dMod::profile
#' @param value_name Choose from numerical attributes of obj, e.g. "value", "data", "prior" ...
#'
#' @return data.table(whichPar, profileDirection, isOpen)
#' @export
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @family profiles
#'
#' @examples
cf_profile_getOpenProfiles <- function(profiles, value_column = "data") {
  dp <- as.data.table(profiles)
  dp[,`:=`(profileDirection = if(constraint <= 0) "left" else "right"), by = 1:nrow(dp)]
  dp[,`:=`(VALUECOLUMN = eval(parse(text = value_column)))]
  valueOptimum <- dp[constraint == 0, unique(VALUECOLUMN)]
  dp[,`:=`(valueOptimum = valueOptimum)]
  dp[,`:=`(valueDifference = VALUECOLUMN - valueOptimum)]
  
  dpOpen <- dp[,list(isOpen = all(valueDifference < 3.84)), by = c("whichPar", "profileDirection")]
  dpOpen <- dpOpen[isOpen == TRUE]
  dpOpen
}



#' Prepare parameters affected by a profile
#' 
#' Its not helpful to plot all paths in plotPaths, 
#' but only of those parameters which are affected
#'
#' @param dp data.table with profiles
#' @param parnames columns indicating parameter names
#' @param tol tolerance to measure if a parameter is affected
#'
#' @return data.table(value, constraint, PARAMETER1, PARAMETER2, PARVALUE1, PARVALUE2)]
#' @export
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
cf_profile_prepareAffectedPaths <- function(profiles, tol = 1e-1) {
  parnames <- cf_parf_parNames(profiles)
  dp <- data.table(profiles)
  dp <- split(dp, dp$whichPar)
d <- (dp)[[1]]
  dp <- lapply(dp, function(d) {
    wP <- unique(d$whichPar)
    d[,`:=`(PARAMETER1 = whichPar)]
    d[,`:=`(PARVALUE1  = .SD[[1]]), .SDcols = wP]
    d <- melt(d, measure.vars = setdiff(parnames, wP), 
              variable.name = "PARAMETER2", variable.factor = FALSE, 
              value.name = "PARVALUE2")
    d <- d[,list(value, constraint, PARAMETER1, PARAMETER2, PARVALUE1, PARVALUE2)]
    d <- d[,`:=`(AFFECTED = diff(range(PARVALUE2)) > tol), by = c("PARAMETER1", "PARAMETER2")]
    d <- d[AFFECTED == TRUE]
    d[,`:=`(AFFECTED = NULL)]
    d
  })
  dp <- rbindlist(dp, use.names = TRUE)
  dp
}

#' Title
#'
#' @param profiles 
#'
#' @return
#' @export
#'
#' @examples
cf_profile_getStartPar <- function(profiles) {
  parnames <- cf_parf_parNames(profiles)
  profopt <- profiles[profiles$constraint == 0, parnames]
  profopt <- as.data.frame(profopt)
  profopt <- unique(profopt)
  profopt <- unlist(profopt)
  profopt <- data.table(PARAMETEROPT = names(profopt), PARVALUEOPT = profopt)
  profopt
}

#' Title
#'
#' @param profiles 
#'
#' @return
#' @export
#'
#' @examples
cf_profile_getOptimum <- function(profiles) {
  parnames <- cf_parf_parNames(profiles)
  profopt <- profiles[which.min(profiles$value), parnames]
  profopt <- as.data.frame(profopt)
  profopt <- unique(profopt)
  if (nrow(profopt) > 1) {
    warning("Multiple parameter sets with optimum value. Taking the first one")
    profopt <- profopt[1,]
  }
  profopt <- unlist(profopt)
  profopt <- data.table(PARAMETEROPT = names(profopt), PARVALUEOPT = profopt)
  profopt
}

#' Title
#'
#' @param profiles profiles to plotPaths
#' @param page pagination page
#' @param tol 
#'
#' @return paginated ggplot
#' @export
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' 
cf_profile_plotPathsAffected <- function(profiles, tol = 1e-1, 
                                         ggCallback = list(facet_wrap_paginate(~PARAMETER1, nrow = 2, ncol = 2, scales = "free", page = 1),
                                                           guides(color = TRUE)),
                                         filename = NULL, FLAGfuture = TRUE,
                                         width = 29.7, height = 21, scale = 1, units = "cm"
                                         ) {
  
  dp <- cf_profile_prepareAffectedPaths(profiles, tol = tol)
  dfit <- dp[cf_profile_getStartPar(profiles), on = c("PARAMETER1" = "PARAMETEROPT", PARVALUE1 = "PARVALUEOPT")]
  dfit <- dfit[!is.na(PARVALUE2)]
  dopt <- dp[cf_profile_getOptimum(profiles), on = c("PARAMETER1" = "PARAMETEROPT", PARVALUE1 = "PARVALUEOPT")]
  dopt <- dopt[!is.na(PARVALUE2)]
  
  pl <- cfggplot(dp, aes(PARVALUE1, PARVALUE2, group = PARAMETER2, color = PARAMETER2)) + 
    geom_line() + 
    geom_point(data = dfit, size = 2) + 
    geom_point(data = dopt, shape = 4, size = 2) 
  for (plx in ggCallback) pl <- pl + plx
  
  message("Plot has ", n_pages(pl), " pages.\n")
  
  cf_outputFigure(pl = pl, filename = filename, width = width, height = height, scale = scale, units = units, FLAGFuture = FLAGfuture)
}



# ---------------------------------------------------------- #
# Data ----
# ---------------------------------------------------------- #


#' create vector of times from min to max
#'
#' @param data datalist
#'
#' @export
datatimes <- function(data, n = 100){
  mytimes <- data %>% as.data.frame() %>% .$time %>% unique
  out <- mytimes %>% range %>% setNames(c("from", "to")) %>%
    c(length.out = n) %>% as.list %>% do.call(seq,.) %>%
    c(mytimes) %>% sort
  return(out)
}


#' Identity function for datalists
#'
#' @param x .
#' @param ... .
#' @export
#' 
#' @importFrom dMod as.datalist
as.datalist.datalist <- function(x,...){
  dMod:::as.datalist.list(x,...)
}


#' @export
#' @rdname as.datalist.datalist
cf_as.datalist <- function (x, split.by = "condition", keep.covariates = NULL, ...) 
{
  x <- as.data.frame(x)
  x <- dMod:::sanitizeData(x) #hack
  dataframe <- x[["data"]]
  standard.names <- x[["columns"]]
  all.names <- colnames(dataframe)
  if (is.null(split.by)) 
    split.by <- setdiff(all.names, standard.names)
  keep.covariates <- setdiff(names(dataframe), c(standard.names))
  conditions <- lapply(split.by, function(n) dataframe[, n])
  splits <- do.call(paste, c(conditions, list(sep = "_")))
  conditionframe <- dataframe[!duplicated(splits), union(split.by, keep.covariates), drop = FALSE]
  rownames(conditionframe) <- splits[!duplicated(splits)]
  conditionframe$condition <- rownames(conditionframe)
  
  dataframe <- cbind(data.frame(condition = splits), dataframe[, standard.names])
  out <- lapply(unique(splits), function(s) dataframe[dataframe[, 1] == s, -1])
  names(out) <- as.character(unique(splits))
  out <- as.datalist(out)
  attr(out, "condition.grid") <- conditionframe
  return(out)
}



# Compare ----



#' compare two objlists
#'
#' @param .x,.y objlist
#'
#' @return difference of their values
#' @export
compare_objlist <- function(.x,.y) {
  map2(.x,.y, subtract_by_name)
}

# Trust blather analysis ----

#' evaluate objfun along an argpath of a fit
#'
#' @param est a dMod.frame
#' @param hypothesis 1
#' @param fit a fit with blather = T, has to have the same pars as est$obj
#'
#' @return list of objlists
#' @export
trustAna_obj_along_path <- function(est, hypothesis = 1, fit) {
  mypath <- fit$argpath
  
  lapply(1:nrow(mypath), function(i) {
    mypars <- mypath[i,]
    names(mypars) <- names(fit$argument)
    
    with(unlist(est[hypothesis,], F), obj(mypars, fixed = fixed))
  })
}

#' Get the i'th row of an argpath or argtry as a named vector
#'
#'
#' @param fit a trust()-fit with blather = T
#' @param i the iteration (the step taken)
#' @param whichPath argpath or argtry, is regexed. as long as the regex matches uniquely, you can use it, e.g. "p" for path, "tr" for try
#'
#' @return named numeric
#' @export
trustAna_getArg <- function(fit, i = 1, whichPath = "argpath") {
  nm <- names(fit$argument)
  pathnames <- c("argpath", "argtry")
  pathname <- str_subset(pathnames, whichPath)
  out <- fit[[pathname]][i,,drop = T] %>% `names<-`(nm)
  return(out)
}


#' Get blather results in nice data.table
#'
#' @param fit return by dMod::trust(...,blather=TRUE)
#'
#' @return
#' @export
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#'
#' @examples
trustBlatherResults <- function(fit){
  data.table(iteration = 1:length(fit$accept),
             steptype  = fit$steptype,
             accept    = fit$accept,
             valpath   = fit$valpath,
             valtry    = fit$valtry,
             preddiff  = fit$preddiff,
             rho       = fit$rho,
             r         = fit$r,
             stepnorm  = fit$stepnorm
  )
}




# helper functions ----



#' Construct a parframe with tightly sampled parameters around other parameters stored in a parframe
#'
#' @param myparframe a parfame
#' @param n how many new rows should be created per row?
#' @param sd,seed going to rnorm in msParframe
#'
#' @return a parframe without metanames
#' @export
#'
#' @importFrom dMod msParframe as.parvec
#'
#' @examples msNarrow(msParframe(c(a = 0, b = 2), n = 2, sd = 3), sd = 0.1)
msNarrow <- function(myparframe, n = 5, sd = 0.2, seed = NULL) {
  map(1:nrow(myparframe), function(.x) {
    mypars <- as.parvec(myparframe, .x)
    msParframe(mypars, seed = seed, n = n, sd = sd)
  }) %>%
    reduce(bind_rows)
}


#' Set rownames so that the covtable is in condition.grid format
#'
#' @param df a data.frame containing the covariates
#'
#' @export
as.condition.grid <- function(df) {
  mynames <- do.call(paste, c(df, list(sep = "_")))
  
  if (any(duplicated(mynames)))
    stop("Duplicated condition names. Check if conditions can be merged or create additional covariate.")
  `rownames<-`(df, mynames)
}


#' #' Try getting conditions
#' #'
#' #' first from obj, then from data, then from p
#' #'
#' #' @param x dMod frame
#' #' @param hypothesis  1
#' #'
#' #' @export
#' getConditions.tbl_df <- function(x, hypothesis = 1) {
#'   getConditions.fn <- dMod:::getConditions.fn
#'   
#'   if (!is.null(suppressWarnings(x$obj[[hypothesis]])))
#'     return(dMod:::getConditions.fn(x$obj[[hypothesis]]))
#'   if (!is.null(x$data[[hypothesis]]))
#'     return(names(x$data[[hypothesis]]))
#'   if (!is.null(x$p[[hypothesis]]))
#'     return(dMod:::getConditions.fn(x$p[[hypothesis]]))
#' }


#' as.parvec for dMod.frames
#'
#' @param x dMod.frame
#' @param hypothesis 1
#' @param index going to as.parvec.parframe
#' @importFrom dMod as.parvec
#'
#' @export
as.parvec.tbl_df <- function(x, hypothesis = 1, index = 1) {
  x[["parframes"]][[hypothesis]] %>% dMod::as.parvec(index)
}


# d2d data format to dMod data format----

#' Bring wide d2d format of data into long dMod format
#'
#' The challenge is that sigmas get their own column each, therefore it's a bit more complicated than simply gathering
#'
#' @param data a data.frame
#' @param keep character vector of columns with covariates to preserve and not count as variables
#'
#' @return The data in long format
#' @export
d2d2dMod_data <- function(data, keep = NULL) {
  
  # Add a rownumber to uniquely identify rows, when splitting up the  data into values and sigmas
  data <- data %>%
    mutate(.rownumber = 1:nrow(data))
  
  keep0 <- c("time", ".rownumber")
  keep <- c(keep0, keep)
  
  datanames <- names(data)
  sdnames <- datanames %>% str_subset("_sd$")
  varnames <- datanames[!datanames%in%c(keep, sdnames)]
  
  clean_for_output <- . %>%
    select(-.rownumber) %>%
    filter(!is.na(value)) %>%
    as.data.frame()
  
  vardata <- data %>%
    .[c(keep, varnames)] %>%
    gather(name, value, varnames)
  
  if (length(sdnames) == 0) {
    return(vardata %>% clean_for_output)
  }
  
  sddata <- data %>%
    select(UQS(syms(c(keep, sdnames)))) %>%
    gather(name, sigma, UQS(syms(sdnames))) %>%
    mutate(name = str_replace(name, "_sd$", ""))
  
  out <- full_join(vardata, sddata, by = keep) %>%
    clean_for_output
  
  return(out)
}

# ---------------------------------------------------------- #
# Quadratic approximation of fit ----
# ---------------------------------------------------------- #

#' Turn a fit into a 2nd order taylor approximation
#'
#' @param fit output from trust: list with value gradient hessian and argument
#'
#' @return objfun
#' @export
#'
fit2obj <- function(fit) {
  force(fit)
  outfn <- function(pars, fixed = NULL, ...) {
    
    # if (!identical(names(pars), names(fit$argument)))
    #   stop("Parameters have not the same order as in the fit, please rearrange")
    pars <- c(pars, fixed)[names(fit$argument)]
    nm <- setdiff(names(pars), names(fixed))
    value <- fit$value + t(fit$gradient) %*% (pars-fit$argument) + 1/2 * t(pars - fit$argument) %*% fit$hessian %*% (pars - fit$argument)
    gradient <- fit$gradient + fit$hessian %*% (pars - fit$argument)
    gradient <- gradient[,1, drop = TRUE]
    
    out <- objlist(value, gradient[nm], fit$hessian[nm,nm])
    attr(out, "data") <- value
    return(out)
  }
  class(outfn) <- c("objfn", "fn")
  return(outfn)
}

# Clemens Identifiability test ----

#' Clemens Kreutz Quick identifiability test
#'
#' @param model dMod.frame
#' @param hypothesis which hypothesis
#' @param r_test Radius which should be tested
#' @param thresh threshold
#'
#' @return table with objective function values, the fit which is performed during this test and the parameter values.
#' Additional column "identifiable" which is true if the objective function value has been pushed to a value higher than the threshold
#' @export
#'
#' @importFrom dMod as.parvec
#'
#' @examples
#'  \dontrun{DL: To do.}
id_test <- function(model, hypothesis = 1, r_test = log(10), thresh = 1) {
  
  # write out objects to this this environment
  lapply(seq_along(model), function(i) {
    value <- model[[i]][[hypothesis]]
    try(assign(x = paste0(names(model)[i]), value = value, pos = 1), silent = T)})
  
  bestfit <- pars
  if(!is.null(model$parframes))
    bestfit <- parframes %>% dMod:::as.parvec
  
  
  r <- bestfit %>% paste0("(", names(.), " - ", ., ")^2") %>% paste0(collapse = " + ") %>% paste0("sqrt(", ., ")") %>% `names<-`("R_BESTFIT")
  p_id <- P(r)
  constr_id <- constraintL2(c(R_BESTFIT = r_test), sigma = 1, attr.name = "prior_id")
  constr_id <- (constr_id*p_id)
  
  obj_id <- obj + constr_id
  
  current_value <- obj(bestfit, fixed = fixed, deriv = F)$value
  myfit <- trust(obj_id, bestfit, 1, 10, fixed = fixed)
  
  out <- tibble(old_value = current_value,
                new_value = myfit$value,
                new_value_data = attr(myfit, "data"),
                new_value_prior_id  = attr(myfit, "prior_id"),
                new_fit = list(myfit),
                old_argument = list(bestfit),
                new_argument = list(myfit$argument)) %>%
    mutate(identifiable = (new_value-old_value) > thresh)
  
  return(out)
}

# analysis ----

#' Check function to check sensitivities
#'
#'
#' @param prd prediction function
#' @param times vector of times
#' @param pars vector of pars
#' @param whichpar names of pars to check
#' @param cond indexing for condition
#' @param step stepsize for finite difference
#'
#' @details Taken (with slight changes) from Daniel Kaschek in dMod/inst/examples/events.R
#'
#' @family analysisFunctions
#' @seealso [do_checkSensitivities()]
#'
#' @export
checkSensitivities <- function(prd, times, pars, whichpar, cond = 1, step = 0.1) {
  h <- rep(0, length(pars))
  h[which(names(pars) == whichpar)] <- step
  
  
  M1 <-  prd(times, pars, deriv = TRUE)[[cond]]
  M2 <-  prd(times, pars + h, deriv = TRUE)[[cond]]
  M3 <- attr(prd(times, pars, deriv = TRUE)[[cond]], "deriv")
  print(colnames(M3))
  
  S1 <- cbind(time = M1[, 1], (M2[,-1] - M1[,-1])/step)
  # print(colnames(S1))
  S2 <- cbind(time = M1[, 1], M3[,grep(paste0(".", whichpar), colnames(M3), fixed = TRUE)])
  # print(colnames(S2))
  colnames(S1) <- colnames(S2)
  
  out <- list(numeric = S1, sens = S2)
  return(out)
  
}


#' Do Check sensitivities over a list of prds
#'
#' @param prds list of predicition funcitons
#' @param times vector of times
#' @param pars pars
#' @param path filepath for the pdf
#'
#' @seealso [checkSensitivities()]
#' @family doFunctions
#'
#'
#' @export
do_checkSensitivities <- function(prds, times, pars, path = "checkSensitivities.pdf") {
  pdf(path)
  map(prds, function(prd) {
    p1 <- plotCombined((prd)(times, pars, deriv = F))
    
    p2 <- map(names(pars) %>% `names<-`(.,.), function(mypar) {
      out <- checkSensitivities((prd), times, pars, mypar, 3, .0001) %>% as.prdlist
    }) %>%
      transpose() %>%
      map(. %>% do.call(cbind,.) %>% {.[,!duplicated(colnames(.)), drop = F]}) %>%
      as.prdlist() %>%
      plotPrediction()
    
    print(p1)
    print(p2)
    NULL
  })
  dev.off(dev.cur())
}








# Plots ----

#' Look at the argpath of a fit
#'
#' Requires the fit to be run with blather = T.
#' The same functionality was actually already implemented by Malenka in dMod::plot.parlist(..., path = T)
#' Both are nice and you see different things.
#' Maybe it would be cool to implement a clustering-option into Malenkas plot.parlist()
#'
#' @param fit a fit returned from dMod::trust(..., blather = T)
#'
#' @return a data.frame for plotting with plot_argpath
#'
#' @export
prepare_argpath <- function(fit, ncluster = 1) {
  chisquare_values <- data.frame(iteration = 1:length(fit[["valpath"]]), parameter = "-2LogLikelihood", value = log(abs(fit[["valpath"]])), cluster = 0)
  
  fit %>%
    .$argpath %>%
    { myargpath <- .
    myclustering <- myargpath %>% t %>% kmeans(ncluster) %>% .$cluster
    myargpath <- myargpath %>%
      as.data.frame() %>%
      set_names(names(fit$argument))%>%
      cbind(., iteration = 1:nrow(.)) %>%
      gather(.,"parameter", "value", 1:(length(.)-1)) %>%
      cbind(.,cluster = rep(myclustering, each = max(.$iteration)))
    } %>%
    mutate(parameter = parameter %>% as.factor) %>%
    mutate(cluster = as.factor(cluster)) %>%
    rbind(chisquare_values)
  
}

#' @rdname prepare_argpath
#'
#' @param argpath result from \link{prepare_argpath}
#'
#' @export
plot_argpath <- function(argpath) {argpath %>% ggplot(aes(value, parameter, frame = iteration, color = cluster)) +
    geom_point()
}





#' Get the nice plot of fitErrormodel
#'
#' @param data data which goes into fitErrormodel
#' @details This function is mainly to print the plotting function and to copy/paste it for manipulating it in a script
#'
#'
#' @export
fitErrorModel_plot <- function(data) {
  fitErrorModel(data, factors = c("study"), blather = T, plotting = F,
                errorModel = "exp(s0) +exp(srel)*x", par = c(s0 = -4, srel = 0)) %T>% print %>%
    ggplot(aes(x=value)) +
    geom_point(aes(y=sigmaLS^2*(n), color = log(time))) +
    geom_line(aes(y=sigma^2*n)) +
    geom_ribbon(aes(ymin=cbLower95, ymax=cbUpper95), alpha=.3) +
    geom_ribbon(aes(ymin=cbLower68, ymax=cbUpper68), alpha=.3) +
    ylab("variance") +
    facet_wrap(~condidnt, scales = "free") +
    scale_y_log10() +
    theme_dMod() +
    scale_color_continuous( low = "#98f5ff", high = "#4c4cdb")
  
}



# ----------------------------------------------- #
# .. plotData ----
# ----------------------------------------------- #


#' Quickly plot data
#' 
#' Converts data to datalist and then uses plotData. 
#'
#' @param data data.frame at least with standard names name time value sigma and covariates
#' @param ... arguments going to plotData.datalist
#' @param smooth add a geom_smooth layer on top
#' @param se TRUE: add the se of the geom_smoothing
#'
#' @export
plotAsDatalist <- function(data, ..., smooth = T, se = FALSE) {
  myplot <- data %>% as.data.frame() %>% cf_as.datalist()
  myplot <- plotData(myplot, ...)
  if(smooth)
    myplot <- myplot + geom_smooth(se = se)
  myplot
}



#' Copied from dMod with additional title showing covariates
#'
#' @param data,...,scales,facet,transform see dMod::plotData
#'
#' @export
cf_plotData.datalist <- function(data, ..., scales = "free", facet = "wrap", transform = NULL, aesthetics = NULL) {
  
  rownames_to_condition <- function(covtable) {
    out <- cbind(condition = rownames(covtable), covtable, stringsAsFactors = F)
    out <- out[!duplicated(names(out))]
    return(out)}
  covtable <- rownames_to_condition(covariates(data))
  
  data <- lbind(data)
  data <- base::merge(data, covtable, by = "condition", all.x = T)
  data <- as.data.frame(dplyr::filter(data, ...), stringsAsFactors = F)
  data$bloq <- ifelse(data$value <= data$lloq, "yes", "no")
  
  if (!is.null(transform)) data <- coordTransform(data, transform)
  
  if (facet == "wrap"){
    aes0 <- list(x = "time",
                 y = "value",
                 ymin = "value - sigma",
                 ymax = "value + sigma",
                 group = "condition",
                 color = "condition",
                 pch = "bloq")
    aesthetics <- c(aes0[setdiff(names(aes0), names(aesthetics))], aesthetics)
    p <- ggplot(data, do.call("aes_string", aesthetics)) + facet_wrap(~name, scales = scales)}
  if (facet == "grid"){
    aes0 <- list(x = "time", y = "value", ymin = "value - sigma", ymax = "value + sigma", pch = "bloq")
    aesthetics <- c(aes0[setdiff(names(aes0), names(aesthetics))], aesthetics)
    p <- ggplot(data, do.call("aes_string", aesthetics)) + facet_grid(name ~ condition, scales = scales)}
  if (facet == "wrap_plain"){
    aes0 <- list(x = "time", y = "value", ymin = "value - sigma", ymax = "value + sigma", pch = "bloq")
    aesthetics <- c(aes0[setdiff(names(aes0), names(aesthetics))], aesthetics)
    p <- ggplot(data, do.call("aes_string", aesthetics)) + facet_wrap(~name*condition, scales = scales)}
  
  p <- p +
    geom_point() +
    geom_errorbar(width = 0) +
    scale_shape_manual(name = "BLoQ", values = c(yes = 4, no = 19))
  
  if (all(data$bloq %in% "no"))
    p <- p + guides(shape = FALSE)
  
  p <- p + labs(subtitle = paste0("Available covariates: ", paste0(names(covtable), collapse = ", ")))
  p <- p + theme_dMod() + scale_color_dMod()
  attr(p, "data") <- data
  return(p)
  
}



# ----------------------------------------------- #
# .. PlotCombined ----
# ----------------------------------------------- #

#' bla
#' @export
#' @rdname plotCombined
#'
#' @details Works with multiple steps (index as a vector). I recommend then using it with \code{aesthetics = c(linetype= "step_")}
#' to access the step-information
cf_plotCombined.dMod.frame <- function(x, hypothesis = 1, index = 1, ... , plotErrorBands = F) {
  
  dots <- substitute(alist(...))
  message("If you want to subset() the plot, specify hypothesis AND index")
  if(is.character(hypothesis)) hypothesis <- which(x$hypothesis == hypothesis)
  
  data <- x[["data"]][[hypothesis]]
  prediction <- title <- NULL
  if (is.null(x[["parframes"]])) {
    prediction <- x[["prd"]][[hypothesis]](x[["times"]][[hypothesis]],
                                           x[["pars"]][[hypothesis]],
                                           deriv = F,
                                           fixed = x[["fixed"]][[hypothesis]])
    title <- paste(x[["hypothesis"]][[hypothesis]], "initiated with predefined (probably random) parameters")
  }
  
  if (!is.null(x[["parframes"]])) {
    
    # browser()
    
    prediction <- map(index, function(ind) {
      myparvec <- as.parvec(x[["parframes"]][[hypothesis]], index = ind)
      prediction <- x[["prd"]][[hypothesis]](x[["times"]][[hypothesis]],
                                             pars = myparvec,
                                             deriv = F,
                                             fixed = x[["fixed"]][[hypothesis]])
      prediction <- map(prediction, function(.x) {attr(.x, "step") <- ind; .x})
      prediction
    }) %>%
      unlist(F) %>%
      `attr<-`("class", c("prdlist", "list"))
    
    title <- paste0("Indices", paste0(index, collapse = " "))
    if (length(index) > 1){
      myvalue <- x[["parframes"]][[hypothesis]][index, "value"]
      title <- paste0(x[["hypothesis"]][[hypothesis]], "\n",
                      "value = ", round(x[["parframes"]][[hypothesis]][index,"value", drop = T],1), "\n",
                      paste0(paste(names(dots), "=", dots )[-1], collapse = "\n"))}
  }
  
  myplot <- cf_plotCombined.prdlist(prediction, data, ...) +
    ggtitle(label = title)
  
  if (plotErrorBands) {
    predicition_with_error <- NULL
    if (!is.null(x[["e"]][[hypothesis]])){
      
      errorconds <- getConditions(x[["e"]][[hypothesis]])
      errorobs <- getEquations(x[["e"]][[hypothesis]])
      
      predicition_with_error <- prediction[errorconds]
      predicition_with_error <- dMod:::as.data.frame.prdlist(predicition_with_error, data = data[errorconds], errfn = x[["e"]][[hypothesis]])
      predicition_with_error <- subset(predicition_with_error, ...)
      
    }
    myplot <- myplot +
      geom_ribbon(aes(fill = condition), data = predicition_with_error, alpha = 0.15, size = 0) + scale_fill_dMod()
  }
  return(myplot)
}

#' bla
#' @export
#' @rdname plotCombined
cf_plotCombined.prdlist <- function(x, data = NULL, ..., scales = "free", facet = "wrap", transform = NULL, aesthetics = NULL) {
  
  mynames <- c("time", "name", "value", "sigma", "condition", "step_")
  covtable <- NULL
  
  if (!is.null(data)) {
    rownames_to_condition <- function(covtable) {
      out <- cbind(condition = rownames(covtable), covtable, stringsAsFactors = F)
      out <- out[!duplicated(names(out))]
      return(out)}
    covtable <- rownames_to_condition(covariates(data))
    
    data <- lbind(data)
    data <- base::merge(data, covtable, by = "condition", all.x = T)
    data <- dplyr::filter(data, ...)
    data <- as.data.frame(data, stringsAsFactors = F)
    data$bloq <- ifelse(data$value <= data$lloq, "yes", "no")
    data <- cbind(data, step_ = rep(NA, nrow(data)))
    if (!is.null(transform)) data <- coordTransform(data, transform)
  }
  
  if (!is.null(x)) {
    mywide2long <- function(out, keep = 1, na.rm = FALSE) {
      conditions <- names(out)
      outlong <- do.call(rbind, lapply(1:max(c(length(conditions), 1)), function(cond) {
        step <- attr(out[[cond]], "step")
        if (is.null(step))
          step <- NA
        cbind(wide2long.matrix(out[[cond]]), condition = conditions[cond], step_ = step)
      }))
      return(outlong)
    }
    x <- cbind(mywide2long(x), sigma = NA)
    if (!is.null(data)) x <- base::merge(x, covtable, by = "condition", all.x = T)
    x <- as.data.frame(dplyr::filter(x, ...), stringsAsFactors = F)
    x$step_ <- as.factor(x$step_)
    if (!is.null(transform)) x <- coordTransform(x, transform)
  }
  
  total <- rbind(x[, unique(c(mynames, names(covtable)))], data[, unique(c(mynames, names(covtable)))])
  
  
  if (facet == "wrap"){
    aes0 <- list(x = "time", y = "value", ymin = "value - sigma", ymax = "value + sigma", color = "condition")
    aesthetics <- c(aes0[setdiff(names(aes0), names(aesthetics))], aesthetics)
    p <- ggplot(total, do.call("aes_string", aesthetics)) + facet_wrap(~name, scales = scales)}
  if (facet == "grid"){
    aes0 <- list(x = "time", y = "value", ymin = "value - sigma", ymax = "value + sigma")
    aesthetics <- c(aes0[setdiff(names(aes0), names(aesthetics))], aesthetics)
    p <- ggplot(total, do.call("aes_string", aesthetics)) + facet_grid(name ~ condition, scales = scales)}
  if (facet == "wrap_plain"){
    aes0 <- list(x = "time", y = "value", ymin = "value - sigma", ymax = "value + sigma")
    aesthetics <- c(aes0[setdiff(names(aes0), names(aesthetics))], aesthetics)
    p <- ggplot(total, do.call("aes_string", aesthetics)) + facet_wrap(~name*condition, scales = scales)}
  
  if (!is.null(x))
    p <- p +  geom_line(data = x)
  
  if (!is.null(data))
    p <- p +
    geom_point(data = data, aes(pch = bloq)) +
    geom_errorbar(data = data, width = 0) +
    scale_shape_manual(name = "BLoQ", values = c(yes = 4, no = 19))
  
  if (all(data$bloq %in% "no"))
    p <- p + guides(shape = FALSE)
  
  p <- p + labs(subtitle = paste0("Available covariates: ", paste0(names(covtable), collapse = ", ")))
  
  p <- p + theme_dMod() + scale_color_dMod()
  
  attr(p, "data") <- list(data = data, prediction = x)
  return(p)
  
}

# ----------------------------------------------- #
# .. plotMulti ----
# ----------------------------------------------- #

#' PlotMulti
#'
#' @param x dMod.frame or prdfn
#' @param ... additional arguments
#' @param parframe_filter dplyr-like arguments to filter rows of the parframe in parframes columns of dMod.frame
#' @param times,parframe x = prdfn, additional needed arguments to simulate
#' @param data x = prdfn, add data to plots
#' @param colorAsFactor when only few different predictions are shown, one can use this to better discrimate prediciotns
#' 
#' @export
plotMulti <- function(x,...) {
  UseMethod("plotMulti", x)
}


#' @export
#' @rdname plotMulti
plotMulti.tbl_df <- function(x, parframe_filter = NULL, ...) {
  
  prd <- x[["prd"]][[1]]
  times <- x[["times"]][[1]]
  myparframe <- x[["parframes"]][[1]]
  if (!is.null(parframe_filter)){
    par_attributes <- attributes(myparframe)
    myparframe <- filter(as.data.frame(myparframe), eval(parframe_filter))
    par_attributes$row.names <- par_attributes$row.names[1:nrow(myparframe)]
    attributes(myparframe) <-par_attributes
  }
  data <- x[["data"]][[1]]
  plotMulti.prdfn(prd, times, myparframe, data, ...)
}

#' @export
#' @rdname plotMulti
plotMulti.prdfn = function(x, times, parframe, data = NULL, ..., colorAsFactor = F) {
  if (!is.null(data)) {
    rownames_to_condition <- function(covtable) {
      out <- cbind(condition = rownames(covtable), covtable,
                   stringsAsFactors = F)
      out <- out[!duplicated(names(out))]
      return(out)
    }
    
    
    covtable <- rownames_to_condition(covariates(data))
    data <- lbind(data)
    data <- base::merge(data, covtable, by = "condition",
                        all.x = T)
    data <- dplyr::filter(data, ...)
    data <- as.data.frame(data, stringsAsFactors = F)
    data$bloq <- ifelse(data$value <= data$lloq, "yes", "no")
  }
  
  
  prediction = dMod:::predict.prdfn(x, times = times, pars = parframe)
  if(!is.null(data)) {
    prediction <- base::merge(prediction, covtable, by = "condition",
                              all.x = T)
    prediction <- dplyr::filter(prediction, ...)
  }
  p = ggplot(prediction, aes(time, value))
  if (colorAsFactor){
    p <- p + geom_line(aes(color = as.factor(.index), group = as.factor(.index)))
  } else {
    p <- p + geom_line(aes(color = .index, group = .index))
  }
  p <- p + facet_wrap(~ name)
  if (!is.null(data)) {
    p <- p + geom_point(data = data, aes(time, value), inherit.aes = F) + geom_errorbar(data = data, aes(ymin = value-sigma, ymax=value+sigma),
                                                                                        width = 0)
  }
  attr(p, "data") <- list(data = data, prediction = prediction)
  return(p)
}


#' @export
#' @rdname plotMulti
#' @details plotMulti.parframe: don't know why I implemented it...
plotMulti.parframe <- function(x, parframe_filter = NULL, dModframe, ...) {
  dModframe$parframes[[1]] <- x
  plotMulti.tbl_df(dModframe, parframe_filter, ...)
}

# ----------------------------------------------- #
# .. plotPars ----
# ----------------------------------------------- #


#' Plot parameter values for a fitlist
#'
#' @param x parameter frame as obtained by as.parframe(mstrust)
#' @param tol maximal allowed difference between neighboring objective values
#' to be recognized as one.
#' @param ... arguments for subsetting of x
#' @export
plotPars <- function(x,...) {
  UseMethod("plotPars", x)
}



#' @export
#' @rdname plotPars
plotPars.parframe <- function(x, tol = 1, ...){
  
  if (!missing(...)) x <- subset(x, ...)
  
  jumps <- dMod:::stepDetect(x$value, tol)
  jump.index <- approx(jumps, jumps, xout = 1:length(x$value), method = "constant", rule = 2)$y
  
  #values <- round(x$value/tol)
  #unique.values <- unique(values)
  #jumps <- which(!duplicated(values))
  #jump.index <- jumps[match(values, unique.values)]
  x$index <- as.factor(jump.index)
  
  myparframe <- x
  parNames <- attr(myparframe,"parameters")
  parOut <- wide2long.data.frame(out = ((myparframe[, c("index", "value", parNames)])) , keep = 1:2)
  names(parOut) <- c("index", "value", "name", "parvalue")
  plot <- ggplot2::ggplot(parOut, aes(x = name, y = parvalue, color = index)) + geom_boxplot(outlier.alpha = 0) +
    theme_dMod() + scale_color_dMod() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  
  attr(plot, "data") <- parOut
  
  return(plot)
  
}


#' @export
#' @rdname plotPars
#' @param nsteps number of steps from the waterfall plot
plotPars.tbl_df <- function(dMod.frame, hypothesis = 1,  ..., tol = 1 ) {
  
  if (!missing(...)) {dots <- substitute(...)
  } else {
    dots <- T
  }
  
  if(is.character(hypothesis)) hypothesis <- which(dMod.frame$hypothesis == hypothesis)
  
  if (length(hypothesis)>1){
    message("hypothesis must be length 1")
    return(NULL)
  }
  
  myparframe <- dMod.frame[["parframes"]][[hypothesis]] %>% add_stepcolumn(tol = tol)
  
  plotPars.parframe(myparframe, tol = tol, ...)
  
}



# ---------------------------------------------------------- #
# unlink dMod ----
# ---------------------------------------------------------- #

#' Unlink standard dMod-generated files
#'
#' Removes all \*.c and \*.o from your workdir
#'
#' @param endings additional endings
#'
#' @export
unlink_dMod <- function(endings = NULL) {
  unlink(paste0("*.", c("c", "o", endings)))
}






# ---------------------------------------------------------- #
# compile/modelname ----
# ---------------------------------------------------------- #

#' Zip .so files in a file with unique name
#'
#' @param model a dMod.frame
#'
#' @export
#'
protect_so <- function(model) {
  zipname <- format(Sys.time(), "%Y%m%d%H%M%S") %>% paste0("_so_files.zip")
  map(model, function(i) map(i, ~tryCatch(modelname(.x), error = function(e) NULL))) %>%
    unlist() %>% unique() %>% paste0(".so") %>%
    zip(zipname,.)
  invisible(NULL)
}



# Allow NULL in .fn methods----
#' As.datalist method for NULL
#'
#' @param ... NULL
#'
#' @details Not sure if I still need it, it was mainly for the convenient use in methatcetin_fitting.
#'
#' @return NULL
#' @export
as.datalist.NULL <- function( ... ) {
  NULL
}


#' *.fn in functional form for safe evaluation and allowing for p1 or p2 to be NULL
#'
#' @param p1 fn1
#' @param p2 fn2
#'
#' @details Not sure if I still need it, it was mainly for the convenient use in methatcetin_fitting.
#'
#' @export
cfn <- function(p1,p2) {
  if(is.null(p1)|is.null(p2)) {
    return(NULL)
  } else {
    dMod:::`*.fn`(p1,p2)
  }
}






#' normL2 with NULL allowed for data or x
#'
#' @param data datalist
#' @param x prd
#' @param ... other stuff for normL2
#'
#' @details Not sure if I still need it, it was mainly for the convenient use in methatcetin_fitting.
#'
#' @return either NULL or the output of dMod::normL2
#'
#' @examples
#' \dontrun{
#' obj_apap <- cf_normL2(data_apap %>% as.datalist, (g_apap*x_apap*p_apap))
#' obj_apap <- cf_normL2(NULL, (g_apap*x_apap*p_apap))
#' obj_apap <- cf_normL2(data_apap %>% as.datalist, NULL)
#' obj_apap(pars_apap)
#'
#' # why is this?
#' NULL + obj_apap
#' obj_apap + NULL
#' }
cf_normL2 <- function(data, x, ...) {
  if(is.null(data)|is.null(x)) {
    return(NULL)
  } else {
    dMod::normL2(data, x, ...)
  }
}



#' Augmentation of getDerivs to subset wrt to which_states.which_pars
#'
#' @param prediction a prdframe which contains derivatives as an attribute
#' @param which_states which states shall be taken the derivative of ...
#' @param which_pars ... and with regard to which pars?
#'
#'
#' @export
#'
#'
extract_derivs <- function(prediction, which_states = NULL, which_pars = NULL) {
  prediction %>%
    getDerivs() %>%
    lapply(function(i) {
      mynames <-  colnames(i)
      state_columns <- par_columns <- rep(T, length(mynames))
      if(!is.null(which_states)) {
        state_columns <- sapply(which_states, function(ws) {str_detect(mynames, paste0("^", ws, "\\."))}) %>% matrix(ncol = length(which_states)) %>% apply(1, any)
      }
      if(!is.null(which_pars)) {
        par_columns <- sapply(which_pars, function(wp) str_detect(mynames, paste0("\\.", wp, "$"))) %>% matrix(ncol = length(which_pars)) %>% apply(1, any)
      }
      
      cols <- state_columns & par_columns
      cols[1] <- TRUE
      return(i[,cols])
    })
}










