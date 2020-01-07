#' Install keybindings
#'
#' @export
install_cfkeybindings <- function(){
  keybindings_path <- "~/.R/rstudio/keybindings"
  if (!dir.exists(keybindings_path)) dir.create(keybindings_path, FALSE, TRUE)
  keybindings_files <- list.files("inst/keybindings", "json$", F, T)
  lapply(keybindings_files, file.copy, to = keybindings_path, overwrite = TRUE)
  "keybindings installed"
}
