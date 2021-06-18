
#' Title
#'
#' @param fp,type see code of [getPaginateInfo()]
#'
#' @return
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
#' @examples
#' pl <- ggplot(diamonds) +
#'   geom_point(aes(carat, price), alpha = 0.1) +
#'   facet_grid_paginate(color ~ cut:clarity, ncol = 3, nrow = 3, page = 4)
#' fp <- pl$facet$params
#' type <- "facet_grid_paginate"
#' getFacets(fp, type)
getFacets <- function(fp, type) {
  facets <- NULL
  if (type == "facet_grid_paginate"){
    nmr <- ifelse(length(names(fp$rows)), paste0(names(fp$rows), collapse = " + "), ".")
    nmc <- ifelse(length(names(fp$cols)), paste0(names(fp$cols), collapse = " + "), ".")
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
  scales <- switch(as.character(as.numeric(fp$free$x + 2*fp$free$y)), "0" = "fixed", "1" = "free_x", "2" = "free_y", "3" = "free")
  space <- NULL
  if ("space_free" %in% names(fp))
    space  <- switch(as.character(as.numeric(fp$space_free$x + 2*fp$space_free$y)), "0" = "fixed", "1" = "free_x", "2" = "free_y", "3" = "free")
  
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
#' n_pages(pl)
#' length(plotlist)
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
#' Handle paginate automatically, save asynchronously with future
#'
#' @param pl plot like ggplot
#' @param filename,width,height,scale,units,dpi,limitsize,device,... see [ggplot2::ggsave()]
#' @param FLAGFuture Export asynchronously with the future-package
#'
#' @return nothing
#' @export
#' 
#' @importFrom future plan "%<-%"
#' @importFrom grDevices dev.off dev.cur dev.set
#' @importFrom utils capture.output
#' 
#' @details # importFrom ggplot2 plot_dev parse_dpi plot_dim
#' 
#' @example inst/examples/S101-plotting.R
cf_outputFigure <- function(pl, filename = NULL, 
                            width = 16, height = 10, scale = 1, 
                            units = c("cm", "mm", "in")[1], 
                            dpi = 300, limitsize = TRUE, 
                            device = NULL,
                            heightrel = NULL,
                            FLAGFuture = TRUE,
                            FLAGoverwrite = TRUE,
                            ...) {
  
  if (is.null(filename)) return(pl)
  if (!FLAGoverwrite & file.exists(filename)) {
    cat("FLAGoverwrite = FALSE. Plot is not written to disk\n")
    return(pl)
    }
  
  
  if (!is.null(heightrel)) height <- width * heightrel
  
  # Handle paginate: Wraps plot in list of length n_pages. 
  # For unpaginated plots, length(pl)=1
  pl <- cf_applyPaginate(pl) 
  if (length(pl)>1) {
    if(!grepl("pdf$", filename) && !grepl("%03d.png$", filename)) {
      filename <- gsub(".png", "%03d.png", filename)
    }
  } 
  
  # device wrestling
  dpi <- ggplot2:::parse_dpi(dpi)
  dev <- ggplot2:::plot_dev(device, filename, dpi = dpi)
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
  
  if (FLAGFuture) {
    if (!"multisession" %in% class(future::plan())) {
      future::plan("multisession")
      cat("Future plan is now 'multisession'\n")
    }
    message("Temporarily affecting Sys.getenv('OMP_NUM_THREADS')'\n")
    Sys.setenv(OMP_NUM_THREADS = 2)
    future::`%<-%`(.dummy, {doPlot()})
    Sys.setenv(OMP_NUM_THREADS = 1)
  } else {
    doPlot()
  }
  
  invisible(pl)
}


#' Grab a base plot into grid object
#'
#' @return gtree, can be exported with [ggplot2::ggsave()]
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

#' Colors Copied from dMod
#' @export
cfcolorsFULL <- list(
  medium = c(black = '#010202', gray = '#737373', red = '#F15A60', green = '#7AC36A', blue = '#5A9BD4', orange = '#FAA75B', purple = '#9E67AB', maroon = '#CE7058', magenta = '#D77FB4'),
  dark   = c(black = '#010202', gray = '#333333', red = '#EE2E2F', green = '#008C48', blue = '#185AA9', orange = '#F47D23', purple = '#662C91', maroon = '#A21D21', magenta = '#B43894'),
  light  = c(black = '#010202', gray = '#CCCCCC', red = '#F2AFAD', green = '#D9E4AA', blue = '#B8D2EC', orange = '#F3D1B0', purple = '#D5B2D4', maroon = '#DDB9A9', magenta = '#EBC0DA')
)

#' Colors
#' copied and adapted from dMod
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
              "bisque2", "salmon4", "lemonchiffon2", "lightsteelblue", "darkgray", 
              "navajowhite2", "mintcream", "floralwhite", "skyblue1", "seagreen1", 
              "bisque4", "gray19", "gray55", "gray4", "darkorchid1", "mistyrose4", 
              "green3", "deeppink3", "thistle1", "tan1", "lightgoldenrod1", 
              "indianred4", "palegreen", "gray71", "lightgoldenrod4", "turquoise", 
              "springgreen4", "slategray3", "gray65", "orange", "palevioletred3", 
              "paleturquoise", "peachpuff4", "darkolivegreen", "lemonchiffon3", 
              "lightskyblue1", "darkolivegreen3", "gray13", "turquoise2", "gray26",
              rep("gray", 100))



#' Get comprehensive list of all aesthetics accepted by each geom
#'
#' From Moody_Mudskipper at
#' https://stackoverflow.com/questions/11657380/is-there-a-table-or-catalog-of-aesthetics-for-ggplot2
#'
#' License is "CC BY-SA 4.0" according to https://stackoverflow.com/help/licensing
#'
#'
#' @return list(geom_abline = c("slope", "intercept", ...), ...)
#' @export

#'
#' @examples
cfgg_getAllAesthetics <- function() {
  env <- asNamespace("ggplot2")
  all_Geoms <- ls(envir = env, pattern = "^Geom.+")
  all_Geoms <- mget(all_Geoms, env)
  all_aes <- lapply(all_Geoms,function(x) x$aesthetics())
  names(all_aes) <- names(all_aes) %>% substr(5,nchar(.)) %>% tolower() %>% paste0("geom_",.)
  all_aes[!names(all_aes) %in% ls(envir = env)] <- NULL
  all_aes <- lapply(all_aes, function(x) {if("colour" %in% x) x <- c(x, "color"); x}) # color / colour thing. ggplot only returns only one version.
  all_aes
}



#' Center y axes of different facets around 0 or 1
#' 
#' solution adapted from https://stackoverflow.com/questions/31782799/how-to-use-free-scales-but-keep-a-fixed-reference-point-in-ggplot
#' 
#' You need to call this function twice, once with min and once with max
#' 
#' @param bycols character vector of columns to summarize by. These are typically columns you facet over
#' @param fun min or max
#' @param dplot data.table(measurement = nominal values, bycols...). Typically, the data that went into the plot
#' @param FLAGcenterLog do the centering on log scale
#' @param ... arguments to geom_blank
#'
#' @return geom_blank
#' @export
#'
#' @examples
#' 
#' # linear scale
#' dx <- copy(mtcars)
#' dx <- data.table(dx)
#' dx[,`:=`(measurement = wt-mean(wt))]
#' 
#' ggplot(dx, aes(mpg, measurement)) + 
#'   facet_wrap( ~ cyl, scales = "free_y") + 
#'   geom_point() +
#'   scale_centerY("cyl", min, dx, FALSE) +
#'   scale_centerY("cyl", max, dx, FALSE)
#' 
#' # log scale
#' dx <- copy(mtcars)
#' dx <- data.table(dx)
#' dx[,`:=`(measurement = wt/mean(wt))]
#' 
#' ggplot(dx, aes(mpg, measurement)) + 
#'   facet_wrap( ~ cyl, scales = "free_y") + 
#'   geom_point() +
#'   scale_centerY("cyl", min, dx, TRUE) +
#'   scale_centerY("cyl", max, dx, TRUE) + 
#'   scale_y_log10()
#' 
scale_centerY <- function(bycols, fun = min, dplot, FLAGcenterLog = TRUE, ...) {
  dcenter <- data.table(copy(dplot))
  if (FLAGcenterLog) dcenter[,`:=`(measurement = log(measurement))]
  dcenter <- dcenter[,list(measurement = -fun(measurement)), by = bycols]
  if (FLAGcenterLog) dcenter[,`:=`(measurement = exp(measurement))]
  
  geom_blank(data=dcenter, mapping = aes(y=measurement, x=Inf),...)
}




# -------------------------------------------------------------------------#
# theme_msb ----
# -------------------------------------------------------------------------#

# .. msb_dims  -----

#' @export
msb_dims <- list(
  width1col = 8.7,
  width2col = 18,
  heightmax = 23,
  heightlegendmin = 0.7,
  heightlegend1row = 0.3
)

# .. msb_dpi -----

#' @export
msb_dpi <- 1000


#' Title
#'
#' @return
#' @export
#'
#' @examples
theme_msb <- function() {
  theme_classic(base_size = 7, base_family = "Helvetica") + 
    theme(
      text             = element_text(size = 7),
      panel.border     = element_blank(),
      panel.grid       = element_blank(),
      axis.text        = element_text(colour = "black"),
      axis.title       = element_text(face = "plain",colour = "black"),
      axis.ticks       = element_line(color = "black"),
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold")
    )
}


# install_msb_fonts <- function() {
#   # https://forum.ubuntuusers.de/topic/fonts-helvetica-installieren/
#   cat("sudo apt-get install fonts-larabie-deco fonts-larabie-straight fonts-larabie-uncommon")
# }


