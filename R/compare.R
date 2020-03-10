#' Compare function
#'
#' @param x,y vectors to compare
#'
#' @export
compare <- function(x,y, FLAGnames = FALSE, FLAGlengthsOnly = FALSE) {
  
  FO <- FLAGlengthsOnly
  
  if (FLAGnames) {x <- names(x); y <- names(y)}
  cat("\n ======= lengths ========== \n")
  if (!FO) cat(length(x), length(y), sep = ", ")
  cat("\n ======= intersect(x,y): ",length(intersect(x,y))," ========== \n")
  if (!FO) cat(intersect(x,y), sep = ", ")
  cat("\n ======= setdiff(x,y): ",length(setdiff(x,y))," ========== \n")
  if (!FO) cat(setdiff(x,y), sep = ", ")
  cat("\n ======= setdiff(y,x): ",length(setdiff(y,x))," ========== \n")
  if (!FO) cat(setdiff(y,x), sep = ", ")
  out <- list(lengths = c(x = length(x), y = length(y)),
              intersect = intersect(x,y),
              setdiffxy = setdiff(x,y),
              setdiffyx = setdiff(y,x))
}
