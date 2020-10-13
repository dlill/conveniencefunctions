
#' Title
#'
#' @param filename 
#' @param width,height,scale In inches
#' @param ... Arguments going to either [grDevices::pdf()] or [grDevices::png()]
#'
#' @return nothing, called for side effect
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
#'
#' @examples
cf_startDev <- function(filename, width = 14, height = 10, scale = 0.6, ...) {
  width <- width * scale
  height <- height * scale
  if (grepl("png$", filename)) {
    png(filename, width = width, height = height, ...)
  } else if (grepl("pdf$", filename)) {
    pdf(filename, width = width, height = height, ...)
  }
  NULL
}

#' @xport
#' @rdname cf_startDev
cf_stopDev <- function(filename) {
  ending <- substr(filename, nchar(filename)-2, nchar(filename))
  devs <- dev.list()
  for (dev in devs[names(devs) == ending]) dev.off(dev)
  NULL
}

#' Output figures
#' 
#' current features
#' * Add scriptname to plot as small caption
#'
#' @param pl plot like ggplot
#' @param filename 
#' @param scriptname 
#' @param width 
#' @param height 
#' @param scale 
#' @param FLAGaddScriptname 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
cf_outputFigure <- function(pl, filename, scriptname = basename(dirname(filename)), 
                            width = 14, height = 10, scale = 0.6, 
                            FLAGaddScriptname = TRUE, ...) {
  
  cf_startDev(filename, width, height, scale, ...)
  
  print(pl) # cf_printPlot for multipage
  
  if (FLAGaddScriptname)
    grid::grid.text(basename(filename), x = unit(0.02, "npc"), y = unit(0.03, "npc"), hjust = 0, gp = gridgpar(
      fontsize = eval(parse(text = paste0(0.8 * width * scale , " * ", pl$theme$legend.text$size,pl$theme$text$size)))
    ))
  
  cf_stopDev(filename)
  invisible(pl)
}
