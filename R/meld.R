#' Meld two dfs
#' 
#' Write them into tempfiles and then call meld
#' 
#' @param f1,f2 two data.frames
#'
#' @export
meld_dfs <- function(f1,f2) {
  
  tf1 <- tempfile()
  tf2 <- tempfile()
  
  write_csv(f1, tf1)
  write_csv(f2, tf2)
  
  system(paste("meld", tf1, tf2), wait = FALSE)
  
  
}