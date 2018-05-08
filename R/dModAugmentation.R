# Useful print functions for Mathematica ----

#' Print ode vector in mathematica format
#'
#' @param myeqnlist an eqnlist
#'
#' @export
#'
#' @examples
#' NULL %>%
#'   addReaction("A", "B", "k1*A", "conversion A to B") %>%
#'   addReaction("B", "", "k2*B*A", "dedradation of B induced by A") %>%
#'   print_mathematica.eqnlist
print_mathematica.eqnlist <- cf_print_mathematica.eqnlist <- function(myeqnlist) {myeqnlist %>%
    as.eqnvec() %>%
    print_mathematica.eqnvec}

#' Print ode vector in mathematica format
#'
#' @param myeqnvec an eqnvec
#'
#' @export
#'
#' @examples
#' NULL %>%
#'   addReaction("A", "B", "k1*A", "conversion A to B") %>%
#'   addReaction("B", "", "k2*B*A", "dedradation of B induced by A") %>%
#'   as.eqnvec %>%
#'   print_mathematica.eqnlist
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
#'
#' @export
#'
#' @examples
#'  print_mathematica.character(paste0(letters, "_", letters))
print_mathematica.character <- cf_print_mathematica.character <- function(mycharacter) {
  list(
  mycharacter %>%
    paste(collapse = ", ")  %>%
    str_replace_all(c("_" = "")) %>%
    paste0("x={",.,"};")
  ,
  structure(mycharacter,
    names = paste0("\\b", mycharacter %>%
              str_replace_all(c("_" = "")), "\\b"))
  )
  }



# obj_condition_wise ----

#' Evaluate an objective function condition-wise
#'
#' Easily see which condition contributes how much.
#' Be careful to use this in an actual calculation, as in adding the results back together, because then the prior is evaluated multiple times
#'
#' @param obj
#' @param pouter
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
#' @param data
#' @details This function is mainly to print the plotting function and to copy/paste it for manipulating it in a script
#'
#' @return
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
#' @param job
#' @param delta_t
#'
#' @importFrom rlang eval_tidy quo enquo UQ
#' @importFrom beepr beep
#' @export
wait_for_runbg <- function(job, delta_t=5) {

  while(!rlang::eval_tidy(rlang::quo("[["(UQ(rlang::enquo(job)),"check")() )))
    Sys.sleep(delta_t)
  walk(1:10,function(i) {beepr::beep(2); Sys.sleep(0.2)})

}


#' Recover a runbg-jog
#'
#' Recover a runbg-jog when the job is lost somewhere in the abyss between restarting sessions and not saving
#'
#' @param machine What you entered when you started the job
#' @param filename What you entered when you started the job (if you entered nothing look at the RData-files that were generated)
#' @param wait What you entered when you started the job
#'
#' @return the normal return value of runbg
#' @export
#'
#' @author Daniel Kaschek, Daniel Lill just selected the respective lines
recover_runbg <- function(machine, filename, wait = FALSE) {

  nmachines <- length(machine)

  # Set file name
  if (is.null(filename))
    filename <- paste0("tmp_", paste(sample(c(0:9, letters), 5, replace = TRUE), collapse = ""))

  filename0 <- filename
  filename <- paste(filename, 1:nmachines, sep = "_")

  # Initialize output
  out <- structure(vector("list", 3), names = c("check", "get", "purge"))

  # Check
  out[[1]] <- function() {

    check.out <- sapply(1:nmachines, function(m) length(suppressWarnings(
      system(paste0("ssh ", machine[m], " ls ", filename[m], "_folder/ | grep -x ", filename[m], "_result.RData"),
             intern = TRUE))))

    if (all(check.out) > 0) {
      cat("Result is ready!\n")
      return(TRUE)
    }
    else if (any(check.out) > 0) {
      cat("Result from machines", paste(which(check.out > 0), collapse = ", "), "are ready.")
      return(FALSE)
    }
    else if (all(check.out) == 0) {
      cat("Not ready!\n")
      return(FALSE)
    }

  }

  # Get
  out[[2]] <- function() {

    result <- structure(vector(mode = "list", length = nmachines), names = machine)
    for (m in 1:nmachines) {
      .runbgOutput <- NULL
      system(paste0("scp ", machine[m], ":", filename[m], "_folder/", filename[m], "_result.RData ./"), ignore.stdout = TRUE, ignore.stderr = TRUE)
      check <- try(load(file = paste0(filename[m], "_result.RData")), silent = TRUE)
      if (!inherits("try-error", check)) result[[m]] <- .runbgOutput
    }

    .GlobalEnv$.runbgOutput <- result

  }

  # Purge
  out[[3]] <- function() {

    for (m in 1:nmachines) {
      system(paste0("ssh ", machine[m], " rm -r ", filename[m], "_folder"))
    }
    system(paste0("rm ", filename0, "*"))
  }


  # Check if filenames exist and load last result (only if wait == TRUE)
  resultfile <- paste(filename, "result.RData", sep = "_")
  if (all(file.exists(resultfile)) & wait) {

    for (m in 1:nmachines) {

      result <- structure(vector(mode = "list", length = nmachines), names = machine)
      load(file = resultfile[m])
      result[[m]] <- .runbgOutput

    }
    .GlobalEnv$.runbgOutput <- result
    return(out)
  }

  return(out)

}





# Keep Workdir neat and tidy----
#' Remove files with endings .c and .o
#'
#' @param path The path where those files should be removed
#'
#' @export
remove_c_and_o <- function(path = ".") {
  c_and_o <- list.files(path = path, pattern = "(\\.c)|(\\.o)")
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
#' @param p1
#' @param p2
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






