# Ideas ----
#'
#'
#' Ideas for dMod
#'
#' in runbg, when starting eg 20 fits per core and the slowly the load decreases, because there are some fits that take ages, kill these few last fits
#'
#' in normL2, when passing an errmodel with conditions, evalute only those conditions with errmodel and the rest without errmodel
#'
#' plotting_functions: split them into a two-step pipe ("prepare_plotCombined" %>% "render_plotcombined") so that it can be more easily customizable?
#'
#' str_detect_any
#'
#' look at reticulate package


# Plotting ----

#' First way to OO plotting
#'
#' @param plot_bicmet_frame
#' @param hypothesis
#' @param index
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plotCombined.dMod.frame <- function(dMod.frame, hypothesis = 1, index = 1, ... ) {
  if(hypothesis %>% is.character) hypothesis <- which(dMod.frame$hypothesis == hypothesis)
  i <- hypothesis #so i can copy other code

  myparvec <- dMod.frame[i, "parframes"] %>% .[[1]] %>% .[[1]] %>% as.parvec(index = index)

  mypred <- dMod.frame$prd[[i]](times = seq(0, max(as.data.frame(dMod.frame$data[[i]])$time),length.out = 100),
                                       pars = myparvec,
                                       deriv = F)

  myvalue <- dMod.frame[i, "parframes"] %>% .[[1]] %>% .[[1]] %>% .[i, "value"]

  plotCombined.prdlist(mypred,  dMod.frame$data[[i]], ...) + ggtitle(label = paste(dMod.frame$hypothesis[[i]], ",\t", myvalue))

}






# Interaction with .GlobalEnv ----

#' Load one row of a dMod.frame into the .GlobalEnv
#'
#' @param dMod.frame
#' @param hypothesis character or numeric. specifying the name  or the index of the hypothesis
#'
#' @return
#' @export
#'
#' @examples
#' a <- 3
#' myfun <- function(x) {a * x^2}
#' testframe <- tibble(hypothesis = c("apap", "penner"),
#'                     schwurbel = list(qplot(1:10,1:10), "Penis"),
#'                     wupwup = list("moep", faithful),
#'                     myfun = list(function(x) {a * x^2}, myfun),
#'                     a = c(1:2))
#'
#' checkout_hypothesis(testframe, "penner")
#' myfun
#' schwurbel
#' wupwup
#' hypothesis
checkout_hypothesis <- function(dMod.frame, hypothesis, prefix = "", suffix = "") {

  if(is.numeric(hypothesis)) {
    mydMod.frame <- dMod.frame[hypothesis,]
  } else {
    mydMod.frame <- dMod.frame[dMod.frame[["hypothesis"]]==hypothesis,]
  }
  lapply(seq_along(mydMod.frame), function(i)  {
    value <- mydMod.frame[[i]]
    if(is.list(value)&length(value)==1) value = value[[1]]
    try(assign(x = paste0(prefix,names(mydMod.frame)[i],suffix),
               value = value,
               pos = .GlobalEnv),
        silent = T)
  })

  message("Objects in .GlobalEnv are updated")

}



#' Get elements from the .GlobalEnv and turn it into a row of the dMod.frame
#'
#' I don't really use this function, so it might not be worth it to keep working on it.
#'
#' @param dMod.frame the dMod.frame
#' @param exclude not yet implemented
#' @param include not yet implemented
#' @param prefix
#' @param suffix
#'
#' @return the dMod.frame augmented by the new row
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
#' @param dMod.frame
#' @param exclude
#' @param include
#' @param prefix
#' @param suffix
#'
#' @return
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




# Saving/commiting ----

#' Stage a dMod.frame and all of its DLLs
#'
#' @param dMod.frame the dMod.frame or a character vector specifying a RDS-file
#'
#' @return This function is called for its side-effects.
#' @export
#'
#' @importFrom git2r add repository workdir
#'
#' @examples
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
#' @param zipfile If you want to add the files to an existing zipfile, specify the filepath here.
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





