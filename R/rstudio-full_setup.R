#' Quick setup of RStudio on a new machine
#'
#' @param theme_name default: "pastel on dark"
#' 
#' @return called for side-effect
#' @export
cf_install_rstudio <- function(theme_name = c("pastel on dark", "textmate (default)")[1]) {
  # 1. Theme 
  if (theme_name %in% names(rstudioapi::getThemes())) rstudioapi::applyTheme(theme_name)
  # 2. Keybindings
  install_cfkeybindings()
  # 3. Snippets
  install_cfsnippets()  
  # 4. Bash alias for git
  wup <- file.copy(system.file("setup_IQDesktop/bash/bash_aliases", package = "conveniencefunctions"), "~/.bash_aliases", overwrite = FALSE) 
  if (wup) cat("Bash aliases installed \n")
  # 5. Install shortcuts for Thunar
  wup <- file.copy(system.file("setup_IQDesktop/thunar_shortcuts/bookmarks", package = "conveniencefunctions"), "~/.config/gtk-3.0/bookmarks", overwrite = FALSE) 
  if (wup) cat("Explorer shortcuts installed \n")
  # # 6. IQRmate
  # devtools::install_github("IntiQuan/IQRtools", subdir = "IQRmate")
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



#' Install keybindings
#'
#' @export
#' @importFrom stats setNames
install_cfkeybindings <- function(){
  keybindings_path <- "~/.R/rstudio/keybindings"
  if (!dir.exists(keybindings_path)) dir.create(keybindings_path, FALSE, TRUE)
  keybindings_files <- list.files(system.file("setup_IQDesktop/keybindings", package = "conveniencefunctions"), "json$", F, T)
  wup <- vapply(stats::setNames(nm = keybindings_files), file.copy, to = keybindings_path, overwrite = TRUE, FUN.VALUE = TRUE)
  cat(paste0(names(wup)[wup], collapse = " .... \n"),  "installed\n")
}

