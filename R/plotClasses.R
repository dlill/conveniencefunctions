#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plotCombined <- function(x,...) {
  UseMethod("plotCombined", x)
}

#' @export
plotCombined.prdlist <- dMod::plotCombined

#' @rdname plotCombined
#' PlotCombined method fot dMod.frame
#'
#' @param dMod.frame
#' @param hypothesis
#' @param index
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plotCombined.tbl_df <- function(dMod.frame, hypothesis = 1, index = 1, ... ) {

  dots <- substitute(alist(...))

  message("If you want to subset() the plot, specify hypothesis and index")



  if(is.character(hypothesis)) hypothesis <- which(dMod.frame$hypothesis == hypothesis)
  # i <- hypothesis #so i can copy other code

  times <- NULL
  if (!is.null(dMod.frame[["times"]]))
    times <- dMod.frame[["times"]][[hypothesis]]
  else {
    times <- as.data.frame(dMod.frame[["data"]][[hypothesis]])[["time"]]
    times <- seq(min(times), max(times)*1.1, length.out = 100)
    }

  if (is.null(dMod.frame[["parframes"]]))
    return(
      plotCombined.prdlist(
        dMod.frame[["prd"]][[hypothesis]](times, dMod.frame[["pars"]][[hypothesis]], deriv = F),
        dMod.frame[["data"]][[hypothesis]],
        ...) +
        ggtitle(paste(dMod.frame[["hypothesis"]][[hypothesis]], "initiated with predefined (probably random) parameters"))
    )



  myparvec <- as.parvec(dMod.frame[["parframes"]][[hypothesis]], index = index)

  myprediction <- dMod.frame[["prd"]][[hypothesis]](times,
                                pars = myparvec,
                                deriv = F)

  myvalue <- dMod.frame[["parframes"]][[hypothesis]][index, "value"]

  plotCombined.prdlist(myprediction,  dMod.frame[["data"]][[hypothesis]], ...) +
    ggtitle(label = paste0(dMod.frame[["hypothesis"]][[hypothesis]], "\n",
                           "value = ", round(dMod.frame[["parframes"]][[hypothesis]][index,"value", drop = T],1), "\n",
                           paste0(paste(names(dots), "=", dots )[-1], collapse = "\n")) )
}


#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plotData  <- function(x,...) {
  UseMethod("plotData", x)
}

#' @export
plotData.datalist <- dMod::plotData

#' Title
#'
#' @param data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plotData.data.frame <- function(data, ...) {
  plotData.datalist(as.datalist(data))
}

#' @export
plotData.tbl_df <- function(dMod.frame, hypothesis = 1, ... ) {

  dots <- substitute(alist(...))

  if(is.character(hypothesis)) hypothesis <- which(dMod.frame$hypothesis == hypothesis)
  # i <- hypothesis #so i can copy other code

  # myparvec <- as.parvec(dMod.frame[["parframes"]][[hypothesis]], index = index)

  # myprediction <- dMod.frame[["prd"]][[hypothesis]](times = seq(0, max(as.data.frame(dMod.frame[["data"]][[hypothesis]])$time),length.out = 100),
  #                                                   pars = myparvec,
  #                                                   deriv = F)

  # myvalue <- dMod.frame[["parframes"]][[hypothesis]][1, "value"]

  plotData.datalist(dMod.frame[["data"]][[hypothesis]], ...) +
    ggtitle(label = paste0(dMod.frame[["hypothesis"]][[hypothesis]], "\n",
                           "best value = ", round(dMod.frame[["parframes"]][[hypothesis]][1,"value", drop = T],1), "\n",
                           paste0(paste(names(dots), "=", dots )[-1], collapse = "\n")) )
}


# plot.datalist

# plotFluxes <- function(x,...) {
#   UseMethod("plotFluxes", x)
# }

# plot.parlist

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plotPars <- function(x,...) {
  UseMethod("plotPars", x)
}

#' @export
plotPars.parframe <- dMod::plotPars

#' @export
plotPars.tbl_df <- function(dMod.frame, hypothesis = 1, ... ) {

  dots <- substitute(alist(...))

  if(is.character(hypothesis)) hypothesis <- which(dMod.frame$hypothesis == hypothesis)

  plotPars.parframe(dMod.frame[["parframes"]][[hypothesis]], ...) +
    ggtitle(label = paste0(dMod.frame[["hypothesis"]][[hypothesis]], "\n",
                           "best value = ", round(dMod.frame[["parframes"]][[hypothesis]][1,"value", drop = T],1), "\n",
                           paste0(paste(names(dots), "=", dots )[-1], collapse = "\n")) )
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plotPaths <- function(x,...) {
  UseMethod("plotPaths", x)
}

# plot.prdframe

# plot.prdlist

plotPrediction <- function(x,...) {
  UseMethod("plotPrediction", x)
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plotProfile <- function(x,...) {
  UseMethod("plotProfile", x)
}

#' @export
plotProfile.parframe <- dMod::plotProfile

#' @export
plotProfile.list <- dMod::plotProfile

#' @export
plotProfile.tbl_df <- function(dMod.frame, hypothesis = 1, ...) {
  dots <- substitute(alist(...))

  if(is.character(hypothesis)) hypothesis <- which(dMod.frame$hypothesis == hypothesis)

  plotProfile.list(dMod.frame[["profiles"]][[hypothesis]], ...) +
    ggtitle(label = paste0(dMod.frame[["hypothesis"]][[hypothesis]], "\n",
                           "best value = ", round(dMod.frame[["parframes"]][[hypothesis]][1,"value", drop = T],1), "\n",
                           paste0(paste(names(dots), "=", dots )[-1], collapse = "\n")) )
}

plotResiduals <- function(x,...) {
  UseMethod("plotResiduals", x)
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plotValues <- function(x,...) {
  UseMethod("plotValues", x)
}

#' @export
plotValues.parframe <- dMod::plotValues

#' Title
#'
#' @param dMod.frame
#' @param hypothesis
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plotValues.tbl_df <- function(dMod.frame, hypothesis = 1, ... ) {

    dots <- substitute(alist(...))

    if(is.character(hypothesis)) hypothesis <- which(dMod.frame$hypothesis == hypothesis)

    plotValues.parframe(dMod.frame[["parframes"]][[hypothesis]], ...) +
      ggtitle(label = paste0(dMod.frame[["hypothesis"]][[hypothesis]], "\n",
                             "best value = ", round(dMod.frame[["parframes"]][[hypothesis]][1,"value", drop = T],1), "\n",
                             paste0(paste(names(dots), "=", dots )[-1], collapse = "\n")) )
}
