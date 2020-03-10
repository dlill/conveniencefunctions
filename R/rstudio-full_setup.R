#' Quick setup of RStudio on a new machine
#'
#' 
#' @return called for side-effect
#' @export
cf_install_rstudio <- function(FLAGoverwrite = FALSE) {
  # 1. Theme 
  rstudioapi::applyTheme("pastel on dark")
  # 2. Keybindings
  install_cfkeybindings(FLAGoverwrite = FLAGoverwrite)
  # 3. Snippets
  install_cfsnippets(FLAGoverwrite = FLAGoverwrite)  
  # 4. Bash alias for git
  if (Sys.info()["sysname"] == "Windows"){
  wup <- file.copy(system.file("setup_IQDesktop/bash/bash_aliases", package = "conveniencefunctions"), "~/.bash_aliases", overwrite = FLAGoverwrite) 
  system("sed -i 's/\\\\r//' ~/.bash_aliases")
  }
  if (Sys.info()["sysname"] == "Windows"){
    wup <- file.copy(system.file("setup_IQDesktop/bash/bash_aliases", package = "conveniencefunctions"), "~/../.bash_aliases", overwrite = FLAGoverwrite) 
    writeLines("source ~/.bash_aliases", "~/../.bashrc")
  }
  if (wup) cat("Bash aliases installed \n")
  # 5. Install shortcuts for Thunar
  if (!dir.exists("~/.config/gtk-3.0")) dir.create("~/.config/gtk-3.0")
  wup <- file.copy(system.file("setup_IQDesktop/thunar_shortcuts/bookmarks", package = "conveniencefunctions"), "~/.config/gtk-3.0/bookmarks", overwrite = FLAGoverwrite) 
  if (wup) cat("Explorer shortcuts installed \n")
  # # 6. IQRmate
  # devtools::install_github("IntiQuan/IQRtools", subdir = "IQRmate")
}



#' Install keybindings
#'
#' @export
#' @importFrom stats setNames
install_cfkeybindings <- function(FLAGoverwrite = FALSE){
  keybindings_path <- "~/.R/rstudio/keybindings"
  if (!dir.exists(keybindings_path)) dir.create(keybindings_path, FALSE, TRUE)
  keybindings_files <- list.files(system.file("setup_IQDesktop/keybindings", package = "conveniencefunctions"), "json$", F, T)
  wup <- vapply(stats::setNames(nm = keybindings_files), file.copy, to = keybindings_path, overwrite = FLAGoverwrite, FUN.VALUE = TRUE)
  if (any(wup)) cat(paste0(names(wup)[wup], collapse = " .... \n"),  "installed\n")
  NULL
}


#' Title
#'
#' @return
#' @export
#'
#' @examples
install_cfsnippets <- function(FLAGoverwrite = FALSE){
  if(!dir.exists("~/.R/snippets/")) dir.create("~/.R/snippets/", recursive = TRUE)
  wup <- file.copy(system.file("setup_IQDesktop/snippets/r.snippets", package = "conveniencefunctions"), file.path("~/.R/snippets/r.snippets"), overwrite = FLAGoverwrite)
  if (wup) cat("Snippets were overwritten \n")
  NULL
}