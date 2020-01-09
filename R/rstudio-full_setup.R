#' Quick setup of RStudio on a new machine
#'
#' @param theme_name default: "pastel on dark"
#' 
#' @return called for side-effect
#' @export
install_cfrstudio <- function(theme_name = "pastel on dark") {
  
  # 1. Theme name
  if (theme_name %in% names(rstudioapi::getThemes())) rstudioapi::applyTheme(theme_name)
  # 2. Keybindings
  install_cfkeybindings()
  # 3. Snippets
  install_cfsnippets()
  # 4. Bash alias for git
  file.copy(system.file("bash/bash_aliases", package = "conveniencefunctions"), "~/.bash_aliases", overwrite = TRUE)
  
}