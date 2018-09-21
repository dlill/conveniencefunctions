# report generation ----
# dMod.frame <- model

report_dMod.frame <- function(model, folder = tpaste0("report/")) {

  allnames <- names(model)
  nhyp <- nrow(model)

  folder_hypwise <- paste0(folder, "by_hypothesis/")
  folder_objectwise <- paste0(folder, "by_object/")

  dir.create(folder)
  dir.create(paste0(folder_hypwise))
  dir.create(paste0(folder_objectwise))


  # helper functions ----
  get_steps_by_diff <- function(model, hypothesis, min_diff = 0.1, min_stepsize = 3) {
    mysteps <- model$parframes[[hypothesis]]$value %>% diff %>% {.>min_diff} #DL das hier als funktion auch 1 weiter oben
    mysteps[1] <- TRUE
    mysteps[length(mysteps)+1] <- F

    whichstep_bigger_min_stepsize <- which(mysteps) %>% diff() %>% {.>min_stepsize} %>% which
    step_indices <- which(mysteps)[whichstep_bigger_min_stepsize]
  }

  # Table reports ----

  # reactions ----
  report_reactions <- function(model, hypothesis, folder) {
    write.eqnlist(model$reactions[[hypothesis]],file = paste0(folder, "reactions_", hypothesis, ".csv"))
    print(invisible(model$reactions[[hypothesis]])) %>% write_csv(paste0(folder, "reactions_", hypothesis, "_human_readable.csv"))
  }

  # parframes summary ----
  report_parframes_summary <- function(model, hypothesis = NULL, folder) { # hypothesis not used, print all in the same file
    parframes_summary(model, hypothesis = NULL) %>% write_lines(path = paste0(folder, "parframes_summary.csv"))
  }

  # parvecs ----
  # predefined pars, bestfit, best of steps
  report_parvecs <- function(model, hypothesis, folder) {
    step_indices <- get_steps_by_diff(model, hypothesis)

    model$parframes[[hypothesis]] %>% as.data.frame() %>%
      .[step_indices,] %>%
      bind_rows(model$pars[[hypothesis]] %>% t %>% as.data.frame %>% cbind(predefined = "predefined", index = 0, ., stringsAsFactors = F), .) %>%
      write_csv(paste0(folder, "pars_steps.csv"))

  }

  # Plot reports ----
  # data ----
  plot_data <- function(model, hypothesis) {
    p1 <- plotData(model) +
      ggtitle(paste0("Data, hypothesis = ", hypothesis))
    print(p1)

    p2 <- plotData(model) +
      geom_point(aes(color = name)) +
      geom_errorbar(aes(color = name)) +
      facet_wrap(~condition)+
      ggtitle(paste0("Data facet by condition, hypothesis = ", hypothesis))
    print(p2)
  }

  # parframes ----
  png_parframes_scatter <- function(model, hypothesis) {
    fit_value_quantiles <- c("10" = 0.1, "25" = 0.25, "50" = 0.5, "100" = 1)

    walk(seq_along(fit_value_quantiles), function(i) {
      model$parframes[[hypothesis]] %>%
        as.data.frame() %>%
        filter(value < quantile(value, fit_value_quantiles[i])) %>%
        select(-(1:4)) %>%
        pairs() %>% print()
    })
  }

  png_parframes_scatter_step <- function(model, hypothesis) {
  step_indices <- get_steps_by_diff(model, hypothesis)
    walk(seq_along(step_indices), function(i) {
      model$parframes[[hypothesis]] %>%
        as.data.frame() %>%
        filter(value>=value[step_indices[i]]&value<=value[step_indices[i+1]]) %>%
        select(-(1:4)) %>%
        pairs() %>% print()
    })
  }


  plot_values_quantiles <- function(model, hypothesis) {
    fit_value_quantiles <- c("10" = 0.1, "25" = 0.25, "50" = 0.5, "100" = 1)

    walk(seq_along(fit_value_quantiles), function(i) {
      assign("i", i, pos = .GlobalEnv)
      p1 <- plotValues(model, hypothesis, value < quantile(value, fit_value_quantiles[i]))+
        ggtitle(paste0("Pars values, hypothesis = ", hypothesis, "quantiles = ", i))
      print(p1)
    })
  }

  plot_pars_quantiles <- function(model, hypothesis) {
    fit_value_quantiles <- c("10" = 0.1, "25" = 0.25, "50" = 0.5, "100" = 1)

    walk(seq_along(fit_value_quantiles), function(i) {
      assign("i", i, pos = .GlobalEnv)
      p1 <- plotPars(model$parframes[[hypothesis]], tol = 1, value < quantile(value, fit_value_quantiles[i]))+
        ggtitle(paste0("Pars quantiles, hypothesis = ", hypothesis, "quantiles = ", i))

      print(p1)
    })
  }

  plot_pars_stepwise <- function(model, hypothesis) {

    step_indices <- get_steps_by_diff(model, hypothesis)

    walk(seq_along(step_indices), function(i) {
      assign("i", i, pos = .GlobalEnv)
      mystep_indices <- c(step_indices, model$parframes[[hypothesis]] %>% nrow)
      myindices <- seq(mystep_indices[i], mystep_indices[i+1],1)
      p1 <- plotPars(model$parframes[[hypothesis]], tol = 1, value>=value[step_indices[i]]&value<=value[step_indices[i+1]])+
        ggtitle(paste0("Pars stepwise, hypothesis = ", hypothesis, "step_index = ", step_indices[i],
                       "\n best value in step = ", model$parframes[[hypothesis]]$value[step_indices[i]]))

      print(p1)})
  }

  # Predictions ----
  plot_combined_stepwise <- function(model, hypothesis) {

    step_indices <- get_steps_by_diff(model, hypothesis)

    walk(step_indices, function(step_index) {
      assign("step_index", step_index, pos = .GlobalEnv)

      p1 <- plotCombined(model, hypothesis, step_index) +
        ggtitle(paste0("All states and observables, hypothesis = ", hypothesis, "step_index = ", step_index,
                       "\n value = ", model$parframes[[hypothesis]]$value[step_index]))


      print(p1)})
  }


  plot_combined_obs_only_stepwise <- function(model, hypothesis) {

    step_indices <- get_steps_by_diff(model, hypothesis)

    walk(step_indices, function(step_index) {
      assign("step_index", step_index, pos = .GlobalEnv)

      p1 <- plotCombined(model, hypothesis, step_index, name %in% getObservables(model, hypothesis)) +
        ggtitle(paste0("Observables, hypothesis = ", hypothesis, "step_index = ", step_index,
                       "\n value = ", model$parframes[[hypothesis]]$value[step_index]))

      print(p1)})
  }


  # Profiles ----
  plot_profiles <- function(model, hypothesis) {
    if (!is.null(model$profiles[[hypothesis]])) {
      p1 <- plotProfile(model, hypothesis) +
        ggtitle("Profiles, hypothesis = ", hypothesis)
      print(p1)
      }
  }

  # Helper function to execute plots and reports ----
  do_plot <- function(model, folder, plotfun) {
    mypl <- plotfun %>% str_replace("plot_", "")
    plotfun <- get(plotfun)
    pdf(paste0(folder, mypl, ".pdf"))
      map(1:nrow(model), function(hypothesis) {
        plotfun(model, hypothesis)
      })
    dev.off()
  }

  do_png <- function(model, folder, pngfun) {
    mypl <- plotfun %>% str_replace("png_", "")
    pngfun <- get(pngfun)
    map(1:nrow(model), function(hypothesis) {
      png(paste0(folder, mypl, hypothesis, ".png"))
      pngfun(model, hypothesis)
      dev.off()
    })
  }

  do_report <- function(model, folder, reportfun) {
      reportfun <- get(reportfun)
      map(1:nrow(model), function(hypothesis) {
        reportfun(model, hypothesis, folder)
      })
    }

  # Execute plots and other reports ----

  plotfuns <- ls(pattern = "^plot_")
  map(plotfuns, function(plotfun) {
    cat(paste0("doing plots ", plotfun, " --------------------- \n"))
    do_plot(model, folder_hypwise, plotfun)
  })

  pngfuns <- ls(pattern = "^png_")
  map(pngfuns, function(pngfun) {
    cat(paste0("doing pngs ", pngfun, " --------------------- \n"))
    do_png(model, folder_hypwise, pngfun)
  })

  reportfuns <- ls(pattern = "^report")
  map(reportfuns, function(reportfun) {
    cat(paste0("doing report ", reportfun, " --------------------- \n"))

    do_report(model, folder_hypwise, reportfun)
  })

  graphics.off()

}


# dmod.frame building----

#' Simluate data with a dMod.frame
#'
#' @param model dMod.frame, preferably with columns pars and covtable
#' @param hypothesis 1
#' @param output output type. if dMod.frame, the current "data" in the given hypothesis will be overwritten
#' @param timesD,s0,srel,observables things to create the data template
#' @param seed
#'
#' @return either dMod.frame with data in it and column "truth" or data.frame
#' @export
#' @example Examples/SimulateData.R
simulate_data <- function(model,
                          hypothesis = 1,
                          output = c("dMod.frame", "data.frame", "datalist"),
                          timesD = 0:10, s0 = 0.1, srel = 0.1, observables = getObservables(model, hypothesis),
                          seed_pars = 1,  seed_data = 1) {

  conditions <- getConditions(model, hypothesis)
  data_template <- expand.grid(name = observables, time = timesD, s0 = s0, srel = srel, condition = conditions, stringsAsFactors = F)


  pars <- model$pars[[hypothesis]]
  if (is.null(pars)) {
    set.seed(seed_pars)
    pars <- getParameters(model$p[[hypothesis]]) %>% are_names_of(rnorm)
  }

  prd <- (model$g[[hypothesis]] * model$x[[hypothesis]] * model$p[[hypothesis]])
  prediction <- prd(unique(data_template$time), pars, deriv = F) %>% wide2long()

  set.seed(seed_data)
  mydata <- suppressWarnings(left_join(data_template, prediction, by = c("time", "name", "condition"))) %>%
    mutate(sigma = sqrt(s0^2 + srel^2*value^2)) %>%
    mutate(value = value + rnorm(length(value), 0, sigma)) %>%
    select(-s0, -srel)

  covtable <- model$covtable[[hypothesis]]

  if (!is.null(covtable)) {
    rownames_to_condition <- function(covtable) {
      out <- cbind(condition = rownames(covtable), covtable, stringsAsFactors = F)
      out <- out[!duplicated(names(out))]
      return(out)}
    covtable <- rownames_to_condition(covtable)
    mydata <- left_join(mydata, covtable, "condition")
    mydata <- mydata %>% select(-condition) # recreate condition from covariates
  }

  output <- output[1]
  if (output == "dMod.frame") {
    model$data[[hypothesis]] <- mydata %>% as.data.frame %>% as.datalist
    model[["truth"]][[hypothesis]] <- list(pars)
    return(model)
  }

  if(output == "data.frame")
    return(mydata %>% as.data.frame %>% `attr<-`("truth", pars))

  if(output == "datalist")
    return(mydata %>% as.data.frame %>% as.datalist %>% `attr<-`("truth", pars))

}


# Saving/commiting ----

#' Stage a dMod.frame and all of its DLLs
#'
#' @param dMod.frame the dMod.frame or a character vector specifying a RDS-file
#'
#' @return This function is called for its side-effects.
#' @export
#'
#' @importFrom git2r add repository workdir
git_add_dMod.frame <- function(dMod.frame) {
  frame_quo <- enquo(dMod.frame)

  if(is_tibble(dMod.frame)) {
    rds_file <- tpaste0(quo_name(frame_quo), ".rds")
    saveRDS(dMod.frame, rds_file)
  } else if(is.character(dMod.frame)) {
    rds_file <- dMod.frame
    dMod.frame <- readRDS(rds_file)
  } else stop("dMod.frame is neither a file nor a tibble")

  # if (is.null(dMod.frame[["eqnlists"]])) {
  #   print("If called from RMarkdown document, have a look at your console (ctrl+2)")
  #   yn <- readline("Would you like to add a column 'eqnlists'? (y = abort / anything else = continue this function to save without eqnlists)")
  #   if(yn == "y") stop("Commitment has been aborted")
  # }


  models <- do.call(c, dMod.frame) %>%
    map(function(i) {
      mymodelname <- try(modelname(i), silent = T)
      if (!inherits(mymodelname, "try-error")) return(mymodelname)
      else return(NULL)
    }) %>%
    do.call(c,.) %>%
    unique()
  .so <- .Platform$dynlib.ext
  files <- paste0(outer(models, c("", "_s", "_sdcv", "_deriv"), paste0), .so)
  files <- files[file.exists(files)]

  # for compatibility with Rmd which sets its own workdir
  mywd <- getwd()
  mygitrep <- git2r::repository() %>% git2r::workdir()
  subfolder <- str_replace(mywd, mygitrep, "")

  allfiles <- paste0(subfolder, "/", c(files, rds_file))

  walk(allfiles , function(i) git2r::add(git2r::repository(), i))

  NULL
}


#' Zip a dMod.frame and its DLLs
#'
#' This might be useful for collaboration
#'
#' @param dMod.frame The dMod.frame you want to zip
#' @param zipfile If you want to add the files to an existing zipfile, specify the filepath here, else a new file with a timestamp is generated
#'
#' @export
zip_dMod.frame <- function(dMod.frame, zipfile = NULL) {
  frame_quo <- enquo(dMod.frame)

  if(is_tibble(dMod.frame)) {
    rds_file <- tpaste0(quo_name(frame_quo), ".rds")
    saveRDS(dMod.frame, rds_file)
  } else if(is.character(dMod.frame)) {
    rds_file <- dMod.frame
    dMod.frame <- readRDS(rds_file)
  } else stop("dMod.frame is neither a file nor a tibble")

  # if (is.null(dMod.frame[["eqnlists"]])) {
  #   yn <- readline("Would you like to add a column 'eqnlists'? (y = stop this function / anything else = continue this function to save without eqnlists)")
  #   if(yn == "y") stop("Zipping has been aborted")
  # }

  models <- unlist(dMod.frame) %>%
    map(function(i) {
      mymodelname <- try(modelname(i), silent = T)
      if (!inherits(mymodelname, "try-error")) return(mymodelname)
      else return(NULL)
    }) %>%
    do.call(c,.) %>%
    unique()
  .so <- .Platform$dynlib.ext
  files <- paste0(outer(models, c("", "_s", "_sdcv", "_deriv"), paste0), .so)
  files <- files[file.exists(files)]

  if (is.null(zipfile)) {zipfile <- str_replace(rds_file, "rds", "zip")}

  zip(zipfile, c(rds_file, files))
}



# Saving is already solved by saveRDS

#' Load a saved dMod.frame
#'
#' A dMod.frame saved by saveRDS is read and the DLLs required by its fn-objects are loaded as a side-effect
#'
#' @param filename Character, filename e.g. "myframe.rds"
#'
#' a dMod.frame
#' @export
readDMod.frame <- function(filename) {
  frame <- readRDS(filename)
  fns <- c("g", "x", "p", "obj_data", "obj")
  frame[names(frame) %in% fns] %>% unlist(recursive = F) %>% walk(. %>% {try(loadDLL(.), silent = T)})
  return(frame)
}



# Interaction with runbg ----

#' Title
#'
#' @param runbgOutput the .runbgOutput you get when you start a fit_job with insert_runbg on more than one knecht
#' @param return_value one of c("dMod.frame", "parlist"). Do you want the dMod.frame or just the parlist
#' @param whichUnite
#'
#' @export
uniteFits <- function(runbgOutput, whichUnite = c("fits", "parframes"), return_value = c("dMod.frame", "parlist")) {

  return_value <- return_value[1]
  whichUnite <- whichUnite[1]

  if (return_value == "dMod.frame") {
    out <- runbgOutput[[1]]
    if (whichUnite == "fits")
      out[["fits"]] <- runbgOutput %>%
        map("fits") %>%
        transpose %>%
        map(. %>% Reduce(c.parlist, .))
    if (whichUnite == "parframes")
      out[["parframes"]] <- runbgOutput %>%
        map("parframes") %>%
        transpose() %>%
        map(function(parflist) {
          myparframe<-Reduce(rbind,parflist)
          myparframe <- myparframe[order(myparframe$value),]
          myparframe[["index"]] <- 1:nrow(myparframe)
          myparframe})
  } else if (return_value == "parlist") {
    out <- runbgOutput %>% map("fits") %>% transpose() %>% map(. %>% Reduce(c.parlist,.))
  }
  out
}



# Convenience functions for fit result analysis ----

#' Quickly print the number of converged fits
#'
#' @param dMod.frame dMod.frame with parframes column
#' @param hypothesis hypothesis, if null, analyse all hypotheses given in the dMod.frame
#' @param tol tol going to \code{dMod:::stepDetect}
#'
#' @export
parframes_summary <- function(dMod.frame, hypothesis = NULL, tol = 1) {
  if ( is.null(hypothesis)) hypothesis <- 1:nrow(dMod.frame)
  map(hypothesis, function(i) {
    converged <- dMod.frame$parframes[[i]]$converged %>% summary
    values <- dMod.frame$parframes[[i]]$value %>% summary
    steps_first_20 <- dMod.frame$parframes[[i]]$value %>% dMod:::stepDetect(tol) %>% .[1:20]
    largest_steps <- dMod.frame$parframes[[i]] %>% getStepIndices()
    iterations <- dMod.frame$parframes[[i]]$iterations %>% summary

    list(converged = converged,
         values = values,
         steps_first_20 = steps_first_20,
         largest_steps = largest_steps,
         iterations = iterations)
  }) %>% setNames(dMod.frame$hypothesis[hypothesis])
}



# Interaction with .GlobalEnv ----

#' Get elements from the .GlobalEnv and turn it into a row of the dMod.frame
#'
#' I don't really use this function, so it might not be worth it to keep working on it.
#'
#' @param dMod.frame the dMod.frame
#' @param exclude not yet implemented
#' @param include not yet implemented
#' @param prefix prefix
#' @param suffix suffix
#'
#' the dMod.frame augmented by the new row
#' @export
#'
#' @importFrom dplyr bind_rows
#'
checkin_hypothesis <- function(dMod.frame, exclude = NULL, include = NULL, prefix = "", suffix = "") {

  # possible future feature: set those of variables in exclude to list(NULL)
  # another way could be to specify include...

  mynames <- names(dMod.frame)

  new_hypothesis <- lapply(mynames, function(n)  {
    n <- paste0(prefix,n,suffix)
    myobject <- mget(n, envir = .GlobalEnv, ifnotfound = list(NULL))[[1]]
    if (!is.atomic(myobject) | is.null(myobject) | length(myobject)>1) {myobject <- list(myobject)}
    return(myobject)
  })
  names(new_hypothesis) <- mynames

  return(bind_rows(dMod.frame, as.tibble(new_hypothesis)))
}





#' Update a hypothesis
#'
#' @param dMod.frame bla
#' @param exclude bla
#' @param include bla
#' @param prefix bla
#' @param suffix bla
#'
#'
#' @export
#'
#' @examples
#' remove("doedel")
#' dMod.frame <- tibble(hypothesis = "one", doedel = "blabla", wup = "wup")
#'
#' hypothesis = "one"
#' doedel <- "yay"
#' update_hypothesis(dMod.frame)
update_hypothesis <- function(dMod.frame, exclude = NULL, include = NULL, prefix = "", suffix = "") {

  # possible future feature: set those of variables in exclude to list(NULL)
  # another way could be to specify include...

  mynames <- names(dMod.frame)

  new_hypothesis <- lapply(mynames, function(n)  {
    n <- paste0(prefix,n,suffix)
    myobject <- mget(n, envir = .GlobalEnv, ifnotfound = list(NULL))[[1]]
    if (!is.atomic(myobject) | is.null(myobject) | length(myobject)>1) {myobject <- list(myobject)}
    return(myobject)
  })
  names(new_hypothesis) <- mynames

  new_hypothesis <- new_hypothesis[lapply(new_hypothesis, function(i) !is.null(i[[1]])) %>% do.call(c,.)]

  dMod.frame[dMod.frame$hypothesis == new_hypothesis$hypothesis, names(new_hypothesis)] <- new_hypothesis

  return(dMod.frame)
}



