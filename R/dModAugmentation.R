# obj_condition_wise ----

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
#'
#' @export
remove_c_and_o <- function(path = ".") {
  c_and_o <- list.files(path = path, pattern = "(\\.c)|(\\.o)$")
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



