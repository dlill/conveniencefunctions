
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
cf_startDev <- function(filename, width = 14, height = 10, scale = 0.6, units = "cm", ...) {
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



#' Title
#'
#' @param fp,type see code of [getPaginateInfo()]
#'
#' @return
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @examples
getFacets <- function(fp, type) {
  facets <- NULL
  if (type == "facet_grid_paginate"){
    nmr <- ifelse(length(names(fp$rows)), names(fp$rows), ".")
    nmc <- ifelse(length(names(fp$cols)), names(fp$cols), ".")
    facets <- as.formula(paste0(nmr, " ~ ", nmc))
  } else {
    facets <- as.formula(paste0(". ~ ", paste0(names(fp$facets), collapse = " + ")))
  }
  facets
}


#' get the pagination info from a plot
#'
#' @param pl ggplot
#'
#' @return
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
#'
#' @examples
#' pl <- ggplot(diamonds) +
#'   geom_point(aes(carat, price), alpha = 0.1) +
#'   facet_grid_paginate(color ~ cut:clarity, ncol = 3, nrow = 3, page = 4)
#' getPaginateInfo(pl)
#' pl <- ggplot(diamonds) +
#'   geom_point(aes(carat, price), alpha = 0.1) +
#'   facet_grid_paginate(color ~ ., nrow = 2, ncol = 2, page = 1)
#' getPaginateInfo(pl)
#' pl <- ggplot(diamonds) +
#'   geom_point(aes(carat, price), alpha = 0.1) +
#'   facet_grid_paginate( ~ color, nrow = 2, ncol = 2, page = 1)
#' getPaginateInfo(pl)
#' pl <- ggplot(diamonds) +
#'   geom_point(aes(carat, price), alpha = 0.1) +
#'   facet_wrap_paginate( ~ color, nrow = 2, ncol = 2, page = 1)
#' getPaginateInfo(pl)
getPaginateInfo <- function(pl) {
  
  type <- switch(class(pl$facet)[1], FacetGridPaginate = "facet_grid_paginate", 
                 FacetWrapPaginate = "facet_wrap_paginate", NA)
  if (is.na(type)) return(NA)
  
  facet_paginate <- utils::getFromNamespace(type, "ggforce")
  
  fp <- pl$facet$params
  # recover arguments "facets", "scales", "space"
  facets <- getFacets(fp = fp, type = type)
  scales <- switch(as.character(fp$free$x + 2*fp$free$y), "0" = "fixed", "1" = "free_x", "3" = "free_y", "4" = "free")
  space <- NULL
  if ("space_free" %in% names(fp))
    space  <- switch(as.character(fp$space_free$x + 2*fp$space_free$y), "0" = "fixed", "1" = "free_x", "3" = "free_y", "4" = "free")
  
  # Assemble final arglist
  paginateInfo <- c(list(facets = facets, scales = scales, space = space), fp)
  paginateInfo <- paginateInfo[intersect(names(paginateInfo), names(formals(facet_paginate)))]
  paginateInfo <- paginateInfo[setdiff(names(paginateInfo), "page")]
  # "shrink" is not supported for grid
  # "shrink" and switch is not supported for wrap
  # conveniencefunctions::compare(names(formals(facet_paginate)), names(paginateInfo))
  list(facet_paginate = facet_paginate, paginateInfo = paginateInfo)
}

#' Get list of paginated plots
#' 
#' use ggforce
#' 
#' @param pl (not yet facetted) ggplot
#' @param paginateInfo output from [cf_paginateInfo]
#'
#' @return list of ggplots
#' 
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
#' 
#' @importFrom ggforce facet_wrap_paginate facet_grid_paginate
#'
#' @examples
#' # adapted from ggforce examples
#' pl <- ggplot(diamonds) +
#'   geom_point(aes(carat, price), alpha = 0.1) +
#'   facet_grid_paginate(color ~ cut:clarity, ncol = 3, nrow = 3, page = 4)
#' plotlist <- cf_applyPaginate(pl)
#' plotlist[[1]]
#' plotlist[[5]]
cf_applyPaginate <- function(pl) {
  
  pi <- getPaginateInfo(pl)
  
  if (!is.list(pi)) return(list(pl))
  
  facet_paginate <- pi$facet_paginate
  paginateInfo <- pi$paginateInfo
  n <- ggforce::n_pages(pl)
  lapply(1:n, function(i) {
    pl + {do.call(facet_paginate, c(paginateInfo,list(page = i)))}
  })
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
#' 
cf_outputFigure <- function(pl, filename, scriptname = basename(dirname(filename)), 
                            width = 14, height = 10, scale = 0.6, 
                            units = c("in", "cm", "mm"), 
                            dpi = 300, limitsize = TRUE, 
                            FLAGFuture = TRUE,
                            ...) {
  
  
  # Handle paginate: Wraps plot in list of length n_pages. 
  # For unpaginated plots, length(pl)=1
  pl <- cf_applyPaginate(pl) 
  if (length(pl)>1) {
    if(!grepl("pdf$", filename) && !grepl("%03d.png$", filename)) {
      filename <- gsub(".png", "%03d.png", filename)
    }
    FLAGaddScriptname <- FALSE
  } 
  
  # device wrestling
  dpi <- ggplot2:::parse_dpi(dpi)
  dev <- ggplot2:::plot_dev(NULL, filename, dpi = dpi)
  dim <- ggplot2:::plot_dim(c(width, height), scale = scale, units = units, 
                            limitsize = limitsize)
  
  doPlot <- function() {
    old_dev <- grDevices::dev.cur()
    dev(filename = filename, width = dim[1], height = dim[2], ...)
    on.exit(utils::capture.output({
      grDevices::dev.off()
      if (old_dev > 1) grDevices::dev.set(old_dev)
    }))
    for (p in pl) print(p)
    "done"
  }
  
  if (FLAGFuture && !"multisession" %in% class(future::plan())) 
    future::plan("multisession")
  future::`%<-%`(.dummy, {doPlot()})
  # assign(paste0(".dummyplot", round(runif(1),4)), .dummy, .GlobalEnv) # so that the reference is not deleted and future evaluates until the end
  
  invisible()
}


#' Grab a base plot into grid object
#'
#' @return gtree
#' @export
#' @importFrom grid grid.grab
#' @importFrom gridGraphics grid.echo
cfgrab <- function() {
  gridGraphics::grid.echo()
  grid::grid.grab()
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
theme_cf <- function(base_size = 11, base_family = "", FLAGbold = TRUE) {
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
          axis.text.x = element_text(margin=unit(c(3, 3, 0, 3), "mm")),
          axis.text.y = element_text(margin=unit(c(3, 3, 3, 0), "mm")),
          axis.ticks = element_line(colour = "black"),
          axis.ticks.length = unit(-2, "mm"),
          legend.key = element_rect(colour = NA),
          panel.border = element_rect(colour = "black"),
          panel.grid = element_blank(),
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
#' @examples 
#' ggplot(data.frame(x = 1:30,color = factor(cfcolors[1:30], unique(cfcolors[1:30])))) + 
#'   geom_tile(aes(x=x, y = 1, fill = color)) + 
#'   scale_color_manual(values= setNames(nm = cfcolors),
#'   aesthetics = c("color", "fill"))
#' @export
cfcolors <- c("#000000", "#C5000B", "#0084D1", "#579D1C", "#FF950E", 
              "#4B1F6F", "#CC79A7","#006400", "#F0E442", "#8B4513",
              "salmon", "slateblue1", "chocolate3", "firebrick", 
              "cyan3", "chartreuse4", "gold", "ivory4", "seagreen3", "dodgerblue",
              rep("gray", 100))

