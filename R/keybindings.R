#' Install keybindings
#'
#' @export
install_IQRkeybindings <- function(){
  keybindings_path <- "~/.R/rstudio/keybindings"
  keybindings_files <- list.files("inst/keybindings", "json$", F, T)
  lapply(keybindings_files, file.copy, to = keybindings_path, overwrite = TRUE)
}
