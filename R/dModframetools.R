# Ideas ----
#'
#' Plotting functions as methods, eg plotCombined.dMod.frame
#'
#' Combining dMod.frames should be easy as well
#'
#' dMf_append_plots mit subset-möglichkeit, best_prediction erst hier, nicht in dMf_expand_fits
#'
#' rownames for dMod.frames
#'
#' for each expand/append function, write a shrink function which removes the respective columns for saving fits and so on.
#'
#'
#'
#'
#' Ideas for dMod
#'
#' in runbg, when starting eg 20 fits per core and the slowly the load decreases, because there are some fits that take ages, kill these few last fits
#'
#' in norml2, compute the predictions with times individually -> ask Marcus, hat bei mir nicht so viel gebracht.
#'
#' in +.objfn fix the condition-wise thing
#'
#' in objfn make a condition_value attribute?
#'
#' in normL2, when passing an errmodel with conditions, evalute only those conditions with errmodel and the rest without errmodel
#'
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

  plotCombined(mypred,  dMod.frame$data[[i]], ...) + ggtitle(label = paste(dMod.frame$hypothesis[[i]], ",\t", myvalue))

}



#' Add plots to the dMod.frame
#'
#' @param dMod.frame with cols x, parframes, data, best_prediction, hypothesis, best_value, conditions (optional)
#'
#' @return dMod.frame with plots plot_value, plot_pars, plot_combined
#' @export
#'
#' @examples
dMf_append_plots <- function(mydMod.frame, tol = 1, index = 1, ...) {

  message("Please write down what you see")

  # Append waterfall, pars, and prediction of the best fit
  mydMod.frame <- mydMod.frame %>%

    mutate(best_prediction = map(seq_along(x), function(i) {
      prd[[i]](times = seq(0, max(as.data.frame(data[[i]])$time),length.out = 100),
               pars = parframes[[i]] %>% as.parvec(index),
               deriv = F)
    })) %>%

    mutate(plot_value = map(parframes, plotValues, tol = tol),
           plot_pars = map(parframes, plotPars, tol = tol),
           plot_combined = map(seq_along(x), function(i) plotCombined(best_prediction[[i]], data[[i]],...))) %>%
    mutate(plot_value = map(seq_along(x), function(i) plot_value[[i]] + ggtitle(label = paste(hypothesis[[i]], ",\t", best_value[[i]]))),
           plot_pars = map(seq_along(x), function(i) plot_pars[[i]] + ggtitle(label = paste(hypothesis[[i]], ",\t", best_value[[i]]))),
           plot_combined = map(seq_along(x), function(i) plot_combined[[i]] + ggtitle(label = paste(hypothesis[[i]],
                                                                                                    ",\t fit_index:",
                                                                                                    index,
                                                                                                    "\t value:",
                                                                                                    parframes[[i]][index, "value"]))))

  # Adjust labels
  if ("conditions" %in% names(mydMod.frame)) {
    mydMod.frame <- mydMod.frame %>%
      mutate(plot_combined = map(seq_along(x), function(i) plot_combined[[i]] + scale_color_dMod(labels = conditions[[i]])))
  }

  return(mydMod.frame)
}




# Expand fits ----

#' Expand a dMod.frame with parlist by columns derived from these fits
#'
#' @description
#'
#' @param dMod.frame with columns x, data (list of datalists), prd
#'
#' @return dMod.frame augmented by columns parframes, best_parvec_best_value, best_prediction
#' @export
#'
#' @examples
dMf_expand_fits <- function(dMod.frame) {

  if(!any(c("parframes", "fits") %in% names(dMod.frame)))
    stop("either fits or parframes have to be present in dMod.frame")

  if(! "parframes" %in% names(dMod.frame)) {
    dMod.frame <- dMod.frame %>%
      mutate(parframes = map(fits, as.parframe))
  }

   dMod.frame %>%
    mutate(best_parvec = map(parframes, as.parvec)) %>%
    mutate(best_value = map(parframes, function(pf) min(pf$value))) %>%
    mutate(best_prediction = map(seq_along(x), function(i) {
      prd[[i]](times = seq(0, max(as.data.frame(data[[i]])$time),length.out = 100),
               pars = best_parvec[[i]],
               deriv = F)
    }))
}

# Shrink functions ----

#' Remove cols from dMod.frame
#'
#' @param dMod.frame
#'
#' @return
#' @export
#'
#' @examples
shrink_fits <- function(dMod.frame) {
  indices <- which(names(dMod.frame) %in% c("fits", "best_parvec", "best_value", "best_prediction"))
  dMod.frame[-indices]
}


#' @export
#' @rdname shrink_fits
shrink_plots <- function(dMod.frame) {
  indices <- which(names(dMod.frame) %in% c("plot_value", "plot_combined", "plot_pars", "plot_profile"))
  dMod.frame[-indices]
}


# dMod.frame Example? -----
#'
#' Example of a dMod.frame pipe
#'
#' dMod.frame <- tibble(hypothesis = c("cell specific pars with prior", "no cell specifc pars, no prior"),
#' x = list(x,x),
#' g = list(g,g),
#' p = list(p_1, p_2),
#' pars = list(pars_1, pars2),
#' data = list(mydatalist, mydatalist)) %>%
#'   mutate(prd = list((g*x*p_1), (g*x*p_2))) %>%
#'   mutate(prior = list(constraintL2(mu1, sigma1), NULL)) %>%
#'   mutate(obj = lapply(seq_along(x), function(i) normL2(data[[i]], prd[[i]]) + prior[[i]])) %>%
#'   mutate(machine = list(c("knecht1", "knecht2"), c("knecht3"))) %>%
#'   mutate(fits = lapply(seq_along(x), function(i) runbg(mstrust(obj[[i]], pars[[i]]), machine = machine[[i]]) ))
#' # wait a little
#' dMod.frame <- dMod.frame %>%
#'   mutate(fits = lapply(seq_along(x), function(i) fits[[i]][["get"]]()[[machine[[i]]]] )) %>%
#'   mutate(fits = lapply(fits, as.parframe)) %>%
#'   mutate(plots = lapply(seq_along(x), function(i) plotCombined(prd(mytimes, as.parvec(fits[[i]]), data = data[[i]])) ))
#'
#'
# dMod.frame0 <- tibble::tibble(
#   hypothesis = vector("character"),
#   g = list(),
#   x = list(),
#   p = list(),
#   prd = list(),
#   data = list(),
#   obj = list(),
#   pars = list(),
#   fits = list()
# )



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
#' @param dMod.frame the dMod.frame
#' @param exclude not yet implemented
#' @param include
#'
#' @return the dMod.frame augmented by the new row
#' @export
#'
#' @examples
#'
#' remove("doedel")
#' dMod.frame <- tibble(doedel = "blabal")
#' checkin_hypothesis(dMod.frame)
#'
#' doedel <- "yay"
#' checkin_hypothesis(dMod.frame)
#'
#' doedel <- function(x) x^2
#' checkin_hypothesis(dMod.frame)
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

  return(rbind(dMod.frame, as.tibble(new_hypothesis)))
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



