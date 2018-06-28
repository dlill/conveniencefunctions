# Saving/Loading ----
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



