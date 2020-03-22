# report generation ----

#' Print plots and shit
#'
#' @description Ideas for the future:
#' 1. plotPaths
#' 2. plotFluxes
#'
#' @param model DL to do
#' @param folder DL to do
#'
#' @export
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
    mypl <- pngfun %>% str_replace("png_", "")
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


  # DL to do fix the need for the assignment
  fit_value_quantiles <- c("10" = 0.1, "25" = 0.25, "50" = 0.5, "100" = 1)
  assign("fit_value_quantiles", fit_value_quantiles, .GlobalEnv)
  assign("hypothesis", 1, .GlobalEnv)
  step_indices <- get_steps_by_diff(model, hypothesis)
  assign("step_indices", step_indices, .GlobalEnv)



  # Execute plots and other reports ----

  plotfuns <- ls(pattern = "^plot_")
  map(plotfuns, function(plotfun) {
    cat(paste0("doing plots ", plotfun, " --------------------- \n"))
    do_plot(model, folder_hypwise, plotfun)
  })

  # DL: Fix pngfuns
  # pngfuns <- ls(pattern = "^png_")
  # map(pngfuns, function(pngfun) {
  #   cat(paste0("doing pngs ", pngfun, " --------------------- \n"))
  #   do_png(model, folder_hypwise, pngfun)
  # })

  reportfuns <- ls(pattern = "^report")
  map(reportfuns, function(reportfun) {
    cat(paste0("doing report ", reportfun, " --------------------- \n"))

    do_report(model, folder_hypwise, reportfun)
  })

  graphics.off()

}


# Modeling helpers ----

#' Toggle attach.input in a dMod.frame
#'
#' Sometimes it is wanted to turn on "attach.input" in a prediction function to look at internal states
#'
#' @param est1 a dMod.frame
#' @param att_in If supllied, overwrites the toggle
#'
#' @return no return, as the dMod.frame is modified inside this function. The function is called for its side effect.
#' @export
toggle_attach.input <- function(est1, att_in = NULL) {
  toggle <- !controls(est1[["g"]][[1]], name = "attach.input")
  controls(est1[["g"]][[1]], name = "attach.input") <- toggle
  if(!is.null(att_in))
    controls(est1[["g"]][[1]], name = "attach.input") <- att_in
  NULL
}


#' Helper function to reproducibly construct parframes
#'
#' @param pars Named vector. Values don't play a role, only names
#' @param n Integer how many lines should the parframe have
#' @param seed Seed for the random number generator
#' @param samplefun random number generator: \code{\link{rnorm}}, \code{\link{runif}}, etc...
#' @param ... arguments going to samplefun
#'
#' @return parframe (without metanames)
#' @export
#'
#' @examples
#' construct_parframe(c(a = 0, b = 100000), 5)
#' construct_parframe(c(a = 0, b = 100000), 5)
construct_parframe <- function(pars, n = 20, seed = 12345, samplefun = rnorm, ...) {
  set.seed(seed)
  rnd <- samplefun(n*length(pars))
  mypars <- matrix(rnd, nrow = n)
  mypars <- `names<-`(as.data.frame(mypars), names(pars))
  parframe(mypars)
}


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
simulate_data <- function(model,
                          hypothesis = 1,
                          output = c("dMod.frame", "data.frame", "datalist"),
                          timesD = 0:10, s0 = 0.1, srel = 0.1, observables = getObservables(model, hypothesis),
                          seed_pars = 1,  seed_data = 1) {

  conditions <- getConditions.tbl_df(model, hypothesis)
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
    select(-s0, -srel) %>%
    filter((!is.na(value))|(!is.nan(value)))

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



#' Test some basic functionalities of a dMod.frame
#'
#' @param model A dMod.frame
#' @param test_plots,test_obj logical
#'
#' @return an augmented dMod.frame containing columns named like "tested_*"
#' @export
#'
#' @examples
#' \dontrun{
#' DL: TODO
#' }
test_dMod.frame <- function(model,
                            test_plots = T,
                            test_obj = T
                            ) {
  # Test some plots
  if (test_plots) {
    cat("plotCombined ----- \n")
    plotCombined(model) %>% print
    cat("plotData ----- \n")
    plotData(model) %>% print
    cat("plotPrediction ----- \n")
    plotPrediction(model) %>% print
  }


  # Test objective function
  if (test_obj) {
    cat("test objfuns ----- \n")
    if (is.null (model$fixed))
      model <- mutate(model, fixed = list(NULL))

    model <- model %>%
      mutate(tested_obj = list(obj(pars, fixed = fixed)))

    cat("print test of objfuns ----- \n")
    map(seq_len(nrow(model)), function(i) {
      print(model$tested_obj[[i]])
    })
  }


  return(model)
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
  curwd <- getwd()
  frame <- readRDS(filename)
  setwd(dirname(filename))
  fns <- c("g", "x", "p", "obj_data", "obj")
  frame[names(frame) %in% fns] %>% unlist(recursive = F) %>% walk(. %>% {try(loadDLL(.), silent = T)})
  setwd(curwd)
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



