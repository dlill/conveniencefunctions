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
  install_cfkeybindings() #update
  # 3. Snippets
  install_cfsnippets()  #update
  # 4. Bash alias for git
  file.copy(system.file("bash/bash_aliases", package = "conveniencefunctions"), "~/.bash_aliases", overwrite = TRUE) #update
  # 5. install shortcuts
}


#' Title
#'
#' @return
#' @export
#'
#' @examples
cf_rstudioThemes_pastelOnDark = function() {
  rstudioapi::applyTheme("pastel on dark")
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
cf_rstudioThemes_textmateDefault = function() {
  rstudioapi::applyTheme("textmate (default)")
}
