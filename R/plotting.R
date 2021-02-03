
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






#' Nice ggplot theme
#' 
#' Taken from dMod
#' 
#' @param base_size,base_family see ?theme_bw
#' @param FLAGbold Make text bold faced
#'
#' @return
#' @export
theme_cf <- function(base_size = 12, base_family = "", FLAGbold = TRUE) {
  colors <- list(
    medium = c(gray = '#737373', red = '#F15A60', green = '#7AC36A', blue = '#5A9BD4', orange = '#FAA75B', purple = '#9E67AB', maroon = '#CE7058', magenta = '#D77FB4'),
    dark = c(black = '#010202', red = '#EE2E2F', green = '#008C48', blue = '#185AA9', orange = '#F47D23', purple = '#662C91', maroon = '#A21D21', magenta = '#B43894'),
    light = c(gray = '#CCCCCC', red = '#F2AFAD', green = '#D9E4AA', blue = '#B8D2EC', orange = '#F3D1B0', purple = '#D5B2D4', maroon = '#DDB9A9', magenta = '#EBC0DA')
  )
  gray <- colors$medium["gray"]
  black <- colors$dark["black"]
  
  out <- theme_bw(base_size = base_size, base_family = base_family) +
    theme(line = element_line(colour = "black"),
          rect = element_rect(fill = "white", colour = NA),
          text = element_text(colour = "black"),
          axis.text = element_text(size = rel(1.0), colour = "black"),
          axis.text.x = element_text(margin=unit(c(4, 4, 0, 4), "mm")),
          axis.text.y = element_text(margin=unit(c(4, 4, 4, 0), "mm")),
          axis.ticks = element_line(colour = "black"),
          axis.ticks.length = unit(-2, "mm"),
          legend.key = element_rect(colour = NA),
          panel.border = element_rect(colour = "black"),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "white", colour = NA),
          strip.text = element_text(size = rel(1.0))
          ) 
  if (FLAGbold) out <- out + theme(text = element_text(face = "bold"))
  out
}


#' cfggplot
#'
#' @param data,mapping see ?ggplot
#' @param FLAGbold Make text bold faced
#'
#' @return ggplot
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
#'
#' @examples
cfggplot <- function(data = NULL, mapping = aes(), FLAGbold = TRUE) {
  ggplot(data,mapping) + 
    theme_cf(FLAGbold = FLAGbold)
}

#' scalecolorcf
#' Copied from dMod
#' @param ... see scale_color_manual
#'
#' @return added to plots
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
scale_color_cf <- function(...) {
  scale_color_manual(..., values = cfcolors)
}

#' Colors
#' @export
cfcolorsFULL <- list(
  medium = c(black = '#010202', gray = '#737373', red = '#F15A60', green = '#7AC36A', blue = '#5A9BD4', orange = '#FAA75B', purple = '#9E67AB', maroon = '#CE7058', magenta = '#D77FB4'),
  dark   = c(black = '#010202', gray = '#333333', red = '#EE2E2F', green = '#008C48', blue = '#185AA9', orange = '#F47D23', purple = '#662C91', maroon = '#A21D21', magenta = '#B43894'),
  light  = c(black = '#010202', gray = '#CCCCCC', red = '#F2AFAD', green = '#D9E4AA', blue = '#B8D2EC', orange = '#F3D1B0', purple = '#D5B2D4', maroon = '#DDB9A9', magenta = '#EBC0DA')
)

#' Colors
#' @export
cfcolors <- c(rep(c("#000000", "#C5000B", "#0084D1", "#579D1C", "#FF950E", 
                  "#4B1F6F", "#CC79A7","#006400", "#F0E442", "#8B4513"),2), 
                  rep("gray", 100))

