# dmod.frame building----

#' Simluate data with a dMod.frame
#'
#' @param model dMod.frame , may be unfinished
#' @param hypothesis 1
#' @param output output type. if dMod.frame, the current "data" in the given hypothesis will be overwritten
#' @param timesD,s0,srel,observables,... things to create the data template
#' @param seed
#'
#' @return either dMod.frame with data in it and column "truth" or data.frame
#' @export
simulate_data <- function(model,
                          hypothesis = 1,
                          output = c("dMod.frame", "data.frame", "datalist"),
                          timesD = 0:10, s0 = 0.1, srel = 0.1, observables = getObservables(model, hypothesis), ...,
                          seed1 = 1, seed2 = time_to_seed()) {

  conditions <- getConditions(model, hypothesis)
  data_template <- expand.grid(name = observables, time = timesD, s0 = s0, srel = srel, condition = conditions, ..., stringsAsFactors = F)

  set.seed(seed1)
  pars <- getParameters(model$p[[hypothesis]]) %>% are_names_of(rnorm)

  prd <- (model$g[[hypothesis]] * model$x[[hypothesis]] * model$p[[hypothesis]])

  prediction <- prd(unique(data_template$time), pars, deriv = F) %>% wide2long()


  set.seed(seed2)

  mydata <- suppressWarnings(left_join(data_template, prediction, by = c("time", "name", "condition"))) %>%
    mutate(sigma = sqrt(s0^2 + srel^2*value^2)) %>%
    mutate(value = value + rnorm(length(value), 0, sigma)) %>%
    select(-s0, -srel)

  if (output == "dMod.frame") {
    model$data[[hypothesis]] <- mydata %>% as.data.frame %>% as.datalist
    model$truth[[hypothesis]] <- list(pars)
    return(model)
  }

  if(ouput == "data.frame")
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
#'
#'
#' @export
#'
#'
uniteFits <- function(runbgOutput, return_value = c("dMod.frame", "parlist")) {

  return_value <- return_value[1]

  if (return_value == "dMod.frame") {
    myframe <- runbgOutput[[1]]
    myframe$fits <- runbgOutput %>% map("fits") %>% transpose() %>% map(. %>% Reduce(c.parlist,.))
    return(myframe)
  } else if (return_value == "parlist") {
    myfits <- runbgOutput %>% map("fits") %>% transpose() %>% map(. %>% Reduce(c.parlist,.))
    return(myfits)
  }
  NULL
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



