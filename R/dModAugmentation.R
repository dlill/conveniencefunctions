
#' Print ode vector in mathematica format
#'
#' @param myeqnlist an eqnlist
#'
#' @return
#' @export
#'
#' @examples
print_mathematica.eqnlist <- cf_print_mathematica.eqnlist <- function(myeqnlist) {myeqnlist %>%
    as.eqnvec() %>%
    print_mathematica.eqnvec}

#' Print ode vector in mathematica format
#'
#' @param myeqnvec an eqnvec
#'
#' @return
#' @export
#'
#' @examples
print_mathematica.eqnvec <- cf_print_mathematica.eqnvec <- function(myeqnvec) {
  myeqnvec %>%
{structure(as.character(.), names = names(.))} %>%
    .[order(names(.))] %>%
    paste(collapse = ", ")  %>%
    str_replace_all(c("_" = "")) %>%
    paste0("f={",.,"};")}


#' Print a character in mathematica format
#'
#' @param mycharacter
#'
#' @return
#' @export
#'
#' @examples
print_mathematica.character <- cf_print_mathematica.character <- function(mycharacter) {
  list(
  mycharacter %>%
    paste(collapse = ", ")  %>%
    str_replace_all(c("_" = "")) %>%
    paste0("x={",.,"};")
  ,
  structure(mycharacter,
    names = mycharacter %>%
              str_replace_all(c("_" = "")))
  )
  }





#' Evaluate an objective function condition-wise
#'
#' Easily see which condition contributes how much.
#' Be careful to use this in an actual calculation, as in adding the results back together, because then the prior is evaluated multiple times
#'
#' @param obj
#' @param pouter
#'
#' @return
#' @export
#'
#' @example Examples/obj_condition_wise.R
obj_condition_wise <- function(obj, pars, ...) {
  myconditions <- getConditions(obj)

  lapply(myconditions, function(cond) {
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
  }) #%>% do.call(rbind,.)
}




#' Look at the argpath of a fit
#'
#' Requires the fit to be run with blather = T
#'
#' @param fit a fit returned from dMod::trust()
#'
#' @return a data.frame for plotting
#' @export
#'
#' @examples
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
#' @return a plot
#' @export
#'
#' @examples
plot_argpath <- function(argpath) {argpath %>% ggplot(aes(value, parameter, frame = iteration, color = cluster)) +
  geom_point()
}



#' Wait for runbg
#'
#' It's better to use \code{runbg(..., wait = T)}, if you want to wait for the results anyway
#'
#' But this was nice for practicing enquo()
#'
#' @param job
#' @param delta_t
#'
#' @return
#' @export
#'
#' @examples
wait_for_runbg <- function(job, delta_t=5) {

  while(!eval_tidy(quo("[["(!!enquo(job),"check")() )))
    Sys.sleep(delta_t)

}

#' Get the nice plot of fitErrormodel
#'
#' @param data
#' @details This function is mainly to print the plotting function and to copy/paste it for manipulating it in a script
#'
#' @return
#' @export
#'
#' @examples
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





#' Insert parameter values from a given table of covariates
#'
#' @description Why "m"insert? - multiple-insert
#'
#' @param trafo
#' @param pars_to_insert
#'
#' @return
#' @export
#'
#' @examples
minsert <- function (trafo, pars_to_insert)
{

  # Best would be to have this option in insert, basically a check, if either expr or pars_to_insert has been supplied

  if (missing(trafo))
    trafo <- NULL
  lookuptable <- attr(trafo, "tree")
  if (is.list(trafo) & is.null(names(trafo)))
    stop("If trafo is a list, elements must be named.")
  if (is.list(trafo) & !all(names(trafo) %in% rownames(lookuptable)))
    stop("If trafo is a list and contains a lookuptable (is branched from a tree), the list names must be contained in the rownames of the tree.")
  if (!is.list(trafo)) {
    mytrafo <- list(trafo)
  }
  else {
    mytrafo <- trafo
  }
  out <- lapply(1:length(mytrafo), function(i) {
    if (is.list(trafo)) {
      mytable <- lookuptable[names(mytrafo)[i], , drop = FALSE]
    }
    else {
      mytable <- lookuptable[1, , drop = FALSE]
    }
    with(mytable, {
      args <- c(list(expr = "name ~ value", trafo = mytrafo[[i]]),
                list(value = unlist(mget(pars_to_insert)), name = pars_to_insert))
      # print(args)
      do.call(repar, args)
    })
  })
  names(out) <- names(mytrafo)
  if (!is.list(trafo))
    out <- out[[1]]
  attr(out, "tree") <- lookuptable
  return(out)
}

#' Insert only for a subset of conditions
#'
#' @param trafo
#' @param expr
#' @param condition_sub
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
insert2 <- insert_sub <- function (trafo, expr, conditions = NULL, ...)
{
  if (missing(trafo))
    trafo <- NULL
  lookuptable <- attr(trafo, "tree")
  if (is.list(trafo) & is.null(names(trafo)))
    stop("If trafo is a list, elements must be named.")
  if (is.list(trafo) & !all(names(trafo) %in% rownames(lookuptable)))
    stop("If trafo is a list and contains a lookuptable (is branched from a tree), the list names must be contained in the rownames of the tree.")
  if (!is.list(trafo)) {
    mytrafo <- list(trafo)
  }
  else {
    mytrafo <- trafo
  }
  dots <- substitute(alist(...))
  out <- lapply(1:length(mytrafo), function(i) {
    if (is.list(trafo)) {
      mytable <- lookuptable[names(mytrafo)[i], , drop = FALSE]
    }
    else {
      mytable <- lookuptable[1, , drop = FALSE]
    }

    if((!is.null(conditions)) & (!str_detect(rownames(mytable), conditions))) {return(mytrafo[[i]])}

    with(mytable, {
      args <- c(list(expr = expr, trafo = mytrafo[[i]]),
                eval(dots))
      do.call(repar, args)
    })
  })
  names(out) <- names(mytrafo)
  if (!is.list(trafo))
    out <- out[[1]]
  attr(out, "tree") <- lookuptable
  return(out)
}




#' As.datalist method for NULL
#'
#' @param ... NULL
#'
#' @return
#' @export
#'
#' @examples
as.datalist.NULL <- function( ... ) {
  NULL
}


#' *.fn in functional form for safe evaluation and allowing for p1 or p2 to be NULL
#'
#' @param p1
#' @param p2
#'
#' @return
#' @export
#'
#' @examples
cfn <- function(p1,p2) {
  if(is.null(p1)|is.null(p2)) {
    return(NULL)
    } else {
  dMod:::`*.fn`(p1,p2)
    }
}






#' normL2 with NULL allowed for data or x
#'
#' @param data
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' obj_apap <- cf_normL2(data_apap %>% as.datalist, (g_apap*x_apap*p_apap))
#' obj_apap <- cf_normL2(NULL, (g_apap*x_apap*p_apap))
#' obj_apap <- cf_normL2(data_apap %>% as.datalist, NULL)
#' obj_apap(pars_apap)
#'
#' # why is this?
#' NULL + obj_apap
#' obj_apap + NULL
cf_normL2 <- function(data, x, ...) {
  if(is.null(data)|is.null(x)) {
    return(NULL)
  } else {
    dMod::normL2(data, x, ...)
  }
}






