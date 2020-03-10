# ggplot2 ----

#' Remove a geom layer
#'
#' @param x ggplot object
#' @param geom_type Character such as "GeomLine"
#' @param last_only only the topmost layer of this kind is removed
#'
#' @details This function was mainly written by Pedro Aphalo on https://stackoverflow.com/questions/13407236/remove-a-layer-from-a-ggplot2-chart#51612148
#' I only added the last_only - argument
#'
#' @return the plot with the layers removed
#' @export
remove_geom <- function(x, geom_type, last_only = T) {
  # Find layers that match the requested type.
  selector <- sapply(x$layers,
                     function(y) {
                       class(y$geom)[1] == geom_type
                     })
  if(last_only & sum(selector) > 0)
    selector <- max(which(selector))
  # Delete the layers.
  x$layers[selector] <- NULL
  x
}


