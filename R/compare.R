#' Compare function
#'
#' @param x,y vectors to compare
#'
#' @export
compare <- function(x,y) {
  cat("\n ======= lengths ========== \n")
  cat(length(x), length(y), sep = ", ")
  cat("\n ======= intersect(x,y) ========== \n")
  cat(intersect(x,y), sep = ", ")
  cat("\n ======= setdiff(x,y) ========== \n")
  cat(setdiff(x,y), sep = ", ")
  cat("\n ======= setdiff(y,x) ========== \n")
  cat(setdiff(y,x), sep = ", ")
}
