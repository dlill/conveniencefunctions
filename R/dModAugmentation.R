# Compare ----

#' Setdiff of names
#'
#' @param .x,.y Vectors/matrices with (dim)names
#'
#' @return output of compare.character
#' @export
compare_names <- function(.x,.y) {
  if(length(dim(.x))!=length(dim(.y)))
    stop("not the same number of dimensions")
  if (is.null(dim(.x))) {
    if(!identical(order(names(.x)), order(names(.y))))
      warning("Names not in identical order")
    return(compare(names(.x), names(.y)))
  }
  if (length(dim(.x)) == 2) {
    if(!(identical(order(rownames(.x)), order(rownames(.y)))&identical(order(colnames(.x)), order(colnames(.y)))))
      warning("Dimnames not in identical order")
    return(map2(dimnames(.x), dimnames(.y), ~compare(.x, .y)))
  }
}

#' compare named numerics
#'
#' @param .x,.y Named numerics
#'
#' @return data.frame with name, diff = (.y-.x), appended output of compare_names
#' @export
compare_named_numeric <- function(.x,.y) {
  if(!identical(order(names(.x)), order(names(.y))))
    warning("Names not in identical order")
  out <- subtract_by_name(.y,.x)
  out <- data.frame(diff = out, name = names(out), stringsAsFactors = F)
  comp <- compare(names(.x), names(.y))
  comp <- data.frame(comp$name, diff = rownames(comp))
  out <- rbind(out, comp)
  rownames(out) <- NULL
  return(out)
}

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

# helper functions ----

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


#' Try getting conditions
#'
#' first from obj, then from data, then from p
#'
#' @export
getConditions.tbl_df <- function(model, hypothesis = 1) {
  if (!is.null(suppressWarnings(model$obj[[hypothesis]])))
    return(dMod:::getConditions.fn(model$obj[[hypothesis]]))
  if (!is.null(model$data[[hypothesis]]))
    return(names(model$data[[hypothesis]]))
  if (!is.null(model$p[[hypothesis]]))
    return(dMod:::getConditions.fn(model$p[[hypothesis]]))
}


#' as.parvec for dMod.frames
#'
#' @param x dMod.frame
#' @param hypothesis 1
#' @param index going to as.parvec.parframe
#'
#' @export
as.parvec.tbl_df <- function(x, hypothesis = 1, index = 1) {
  x[["parframes"]][[hypothesis]] %>% dMod:::as.parvec.parframe(index)
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
#'
#' @importFrom rlang syms UQS
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
    select(UQS(syms(c(keep, varnames)))) %>%
    gather(name, value, UQS(syms(varnames)))

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
#' @examples
#'  \dontrun{DL: To do.}
id_test <- function(model, hypothesis = 1, r_test = log(10), thresh = 1) {

  # write out objects to this this environment
  lapply(seq_along(model), function(i) {
    value <- model[[i]][[hypothesis]]
    try(assign(x = paste0(names(model)[i]), value = value, pos = 1), silent = T)})

  bestfit <- pars
  if(!is.null(model$parframes))
    bestfit <- parframes %>% as.parvec


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

#' Evaluate an objective function condition-wise
#'
#' Easily see which condition contributes how much.
#' Be careful to use this in an actual calculation, as in adding the results back together, because then the prior is evaluated multiple times
#'
#' @param obj objfn
#' @param pouter pars at which obj is evaluated
#'
#' @return A tibble with rows corresponding to conditions and cols to several results of the objective function, such as value, gradient...
#' @export
#'
#' @example Examples/obj_condition_wise.R
obj_condition_wise <- function(obj, pars, constr1 = NULL, constr2 = NULL,...) {
  myconditions <- getConditions(obj)

  out <- lapply(myconditions, function(cond) {
    myvalues <- obj(pars = pars, conditions = cond, ...)

    ndata <- get("data", environment(obj))
    if(is.function(ndata)) ndata <- NULL
      else ndata <- ndata[[cond]] %>% nrow
    if(!is.null(ndata)) reduced_value <- myvalues[["value"]]/ndata
      else reduced_value <- NULL

    attrib <- myvalues %>% attributes()

    mylist <- c(condition = cond,
                myvalues[names(myvalues)!="hessian"],
                ndata = ndata,
                reduced_value = reduced_value,
                attrib[!names(attrib)%in%c("env", "class")]) %>%
      lapply(function(i) {if(length(i)==1) {return(i) } else {return(list(i)) }}) #make tibble-compatible

    return(as_tibble(mylist))
  }) %>% do.call(rbind,.)
  out1 <- out2 <- NULL
  if(!is.null(constr1)) out1 <- c(condition = "Constraint 1", constr1(pars = pars, ...) %>% .[names(.)!="hessian"]) %>%
    lapply(function(i) {if(length(i)==1) {return(i) } else {return(list(i)) }})
  # if(!is.null(constr2)) out2 <- c(condition = "Constraint 2", constr2(pars = pars, ...) %>% .[names(.)!="hessian"]) %>%
  #   lapply(function(i) {if(length(i)==1) {return(i) } else {return(list(i)) }})

 out <- bind_rows(out, out1)#, out2)

}





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









# runbg ----

#' Wait for runbg
#'
#' It's better to use \code{runbg(..., wait = T)}, if you want to wait for the results anyway
#'
#' But this was nice for practicing enquo()
#'
#' @param job a runbg object
#' @param delta_t in seconds, the time interval between job$check()s
#'
#' @importFrom rlang eval_tidy quo enquo UQ
#' @importFrom beepr beep
#' @export
wait_for_runbg <- function(job, delta_t=5) {

  while(!rlang::eval_tidy(rlang::quo("[["(UQ(rlang::enquo(job)),"check")() )))
    Sys.sleep(delta_t)
  beepr::beep(8)

}




# Keep Workdir neat and tidy----
#' Remove files with endings .c and .o
#'
#' @param path The path where those files should be removed
#' @param other_extensions,extensions file extensions
#'
#' @export
remove_c_and_o <- function(other_extensions = NULL, path = ".", extensions = c("c", "o")) {
  myregex <- paste0("(\\.", c(extensions, other_extensions), ")$")
  if (length(myregex) > 1)
    myregex <- myregex %>% paste0(collapse = "|")
  c_and_o <- list.files(path = path, pattern = myregex)
  system2("rm", args = c_and_o)
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



# Experimenting with as.datalist

#' cf_as.datalist
#'
#' @importFrom dMod as.datalist
#'
#' @export
#'
#'
cf_as.datalist <- function(x, split.by = NULL, keep.covariates = NULL, make.names.from = NULL, ...) {

  dataframe <- x

  #remaining.names <- setdiff(names(dataframe), split.by)
  all.names <- colnames(dataframe)
  standard.names <- c("name", "time", "value", "sigma")
  if (is.null(split.by)) split.by <- setdiff(all.names, standard.names)


  conditions <- lapply(split.by, function(n) dataframe[, n])
  splits <- do.call(paste, c(conditions, list(sep = "_")))
  subnames <- do.call(paste, c(conditions[split.by %in% make.names.from], list(sep = "_")))
  if (identical(duplicated(splits), duplicated(subnames))) {
    splits <- subnames
  } else {warning("Supplied make.names.from do not identify all conditions")}




  # condition grid
  conditionframe <- dataframe[!duplicated(splits), c(split.by, keep.covariates), drop = FALSE]
  rownames(conditionframe) <- splits[!duplicated(splits)]


  # data list output
  dataframe <- cbind(data.frame(condition = splits), dataframe[, standard.names])
  out <- lapply(unique(splits), function(s) dataframe[dataframe[, 1] == s, -1])

  names(out) <- as.character(unique(splits))

  out <- as.datalist(out)
  attr(out, "condition.grid") <- conditionframe
  return(out)

}




# Update package versions: dMod, cOde, conveniencefunctions, MRAr----
#' Update versions
#'
#' @param cOde,dMod,cf,MRAr Logicals which packages are updated
#' @export
update_version <- function(cOde = T, dMod = T, cf = T, MRAr = F) {
  devtools::install_github("rpremraj/mailR")
  if(cOde) devtools::install_github("dkaschek/cOde")
  if(dMod) devtools::install_github("dkaschek/dMod")
  if(cf)  devtools::install_github("dlill/conveniencefunctions")
  if(MRAr) devtools::install_github("dlill/MRAr")
}





# hierarchical optimization----


#' Run hierarchical optimization
#'
#' Inspired by Daniel Weindl's poster on SBMC 2018
#'
#' @param dMod.frame a dMod frame
#' @param hypothesis the hypotheses
#'
#' @export
#'
#' @examples
#' \dontrun{
#' source("~/Promotion/Software/dMod/inst/examples/example_dMod.frame/setup.R")
#' myframe1 <- myframe1 %>% appendObj
#' }
hierarchical_trust <- function(dMod.frame, hypothesis = 1, analytic_parms) {


  # old try ----
  # Extract parts of the model where an analytic solution exists
  # source("~/Promotion/Software/dMod/inst/examples/example_dMod.frame/setup.R")
  # myframe1 <- myframe1 %>% appendObj
  # checkout_hypothesis(myframe1,1)
  # analytic_parms <- paste0("s1_", letters[1:2])


  # implementing the analytic solution is probably kanonen auf spatzen,
  # since they are rather simple but the equations differ for offsets, scalings and sigmas, especially when they are correlated.
  # Therefore, run a normal optimization but only for the analytic parameters. Convergence should be fast.


  # [] Problem Parameter trafo, passing by x in g*x*p. You have to pass g and p separately?
  # timesD <- lapply(data, `[[`, "time") %>% do.call(c,.) %>% unique()
  # prd <- (x*p)(timesD, pars)
  # p_pars <- p(pars)
  # cn <- getConditions(p)[1]
  # lapply(getConditions(p), function(cn) {
  #     out <- g(prd[[cn]], p_pars[[cn]], fixed = NULL, deriv = TRUE, conditions = cn, env = NULL)
  #     wrss(res(data[[cn]], out[[cn]], err = NULL, loq = loq))
  # })

  # new try ----
  # Old try: implement a new objective function qhich doesn't repeatedly evaluate the ODEs more than once for the optimization of the analytic pars
  # New try: just use trust sequentially alternatingly evaluating one and the other set of pars
  library(dMod)
  library(conveniencefunctions)
  source("~/Promotion/Software/dMod/inst/examples/example_dMod.frame/setup.R")
  myframe1 <- myframe1 %>% appendObj
  checkout_hypothesis(myframe1,1)
  analytic_parms <- paste0("s1_", letters[1:2])
  dynamic_parms <- names(pars) %>% .[!.%in%analytic_parms]



  mypars <- pars
  # optimize the analytic pars
  out_ana <- trust(objfun = obj, mypars[analytic_parms],
        10,10, blather = T,
        fixed = mypars[dynamic_parms])
  mypars[analytic_parms] <- out_ana$argument

  # why is the argument path not linear for the linear model?
  # Because the model is non-linear after the parameter transformation
  mygrid <-  expand.grid(seq(-1,-0,0.05),seq(-2.1,-0.5,0.05)) %>% setNames(analytic_parms)
  i <- mygrid[1,] %>% unlist
  obj_grid <- apply(mygrid, 1, function(i) {
    mypars <- pars
    i <- unlist(i)
    mypars[names(i)] <- i
    out <- data.frame(t(i), value = obj(mypars, deriv = F)$value)
    return(out)
  }) %>%
    do.call(rbind,.)

  argpath <- out_ana$argpath %>% as.data.frame() %>% setNames(analytic_parms)
  obj_grid %>%
    ggplot(aes(s1_a, s1_b, z=value)) +
    geom_contour() +
    geom_point(aes(x = s1_a, y = s1_b, z = NULL), data = argpath)

  mygrid <-  expand.grid(seq(-2.0,-1.75,0.01),seq(-3.1,-2.9,0.01)) %>% setNames(analytic_parms)
  i <- mygrid[1,] %>% unlist
  obj_grid2 <- apply(mygrid, 1, function(i) {
    mypars <- pars
    i <- unlist(i)
    mypars[names(i)] <- i
    out <- data.frame(t(i), value = obj(mypars, deriv = F)$value)
    return(out)
  }) %>%
    do.call(rbind,.)

  argpath <- out_ana$argpath %>% as.data.frame() %>% setNames(analytic_parms)
  obj_grid2 %>%
    ggplot(aes(s1_a, s1_b, z=log(value))) +
    geom_density2d() +
    # geom_point(aes(x = s1_a, y = s1_b, z = NULL), data = argpath)
    geom_blank()

  # why aren't they ellipses? -> look at quadratic objfun in log-coordinates
  mygrid <-  expand.grid(seq(-2.0,-1.75,0.01),seq(-3.1,-2.9,0.01)) %>% setNames(analytic_parms)
  i <- mygrid[1,] %>% unlist
  obj_grid2 <- apply(mygrid, 1, function(i) {
    mypars <- pars
    i <- unlist(i)
    mypars[names(i)] <- i
    out <- data.frame(t(i), value = (exp(i[[1]])-exp(-1.86))^2 + (exp(i[[2]])-2.96)^2)
    return(out)
  }) %>%
    do.call(rbind,.)

  argpath <- out_ana$argpath %>% as.data.frame() %>% setNames(analytic_parms)
  obj_grid2 %>%
    ggplot(aes(s1_a, s1_b, z=log(value))) +
    geom_density2d() +
    # geom_point(aes(x = s1_a, y = s1_b, z = NULL), data = argpath)
    geom_blank()



  # optimize dynamic pars
  out_dyna <- trust(objfun = obj, mypars[dynamic_parms],
                    10,10, blather = T,
                    fixed = mypars[analytic_parms])
  mypars[dynamic_parms] <- out_dyna$argument




}



