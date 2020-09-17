#' Quick setup of RStudio on a new machine
#'
#' 
#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
#' @return called for side-effect
#' @export
cf_install_rstudio <- function(FLAGoverwrite = TRUE, FLAGshortcuts = FALSE) {
  # 1. Theme 
  rstudioapi::applyTheme("pastel on dark")
  # 2. Keybindings
  install_cfkeybindings(FLAGoverwrite = FLAGoverwrite)
  # 3. Snippets
  install_cfsnippets(FLAGoverwrite = FLAGoverwrite)  
  wup <- FLAGoverwrite
  # 4. Bash alias for git
  if (Sys.info()["sysname"] == "Linux"){
    wup <- file.copy(system.file("setup_IQDesktop/bash/bash_aliases", package = "conveniencefunctions"), "~/.bash_aliases", overwrite = FLAGoverwrite) 
    system("sed -i 's/\r//' ~/.bash_aliases")
  }
  if (Sys.info()["sysname"] == "Windows"){
    wup <- file.copy(system.file("setup_IQDesktop/bash/bash_aliases", package = "conveniencefunctions"), "~/../.bash_aliases", overwrite = FLAGoverwrite) 
    writeLines("source ~/.bash_aliases", "~/../.bashrc")
  }
  if (wup) cat("Bash aliases installed \n")
  # 5. Install shortcuts for Thunar
  if (FLAGshortcuts){
    if (!dir.exists("~/.config/gtk-3.0")) dir.create("~/.config/gtk-3.0")
    wup <- file.copy(system.file("setup_IQDesktop/thunar_shortcuts/bookmarks", package = "conveniencefunctions"), 
                     "~/.config/gtk-3.0/bookmarks", overwrite = FLAGoverwrite) 
    if (wup) cat("Explorer shortcuts installed \n")
  }
  # 6. .Rprofile
  file.copy(system.file("setup_IQDesktop/.Rprofile"), "~/.Rprofile")
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

#' Backup the IQRdesktop setup scripts
#'
#' @param path_IQDesktop 
#' @param filename_zip 
#'
#' @return
#' @export
#'
#' @examples
backup_IQdesktopSetupScripts <- function(path_IQDesktop = "/IQDESKTOP/PROJTOOLS/IQDesktop/",
                                         filename_zip = "~/PROJTOOLS/conveniencefunctions/inst/setup_IQDesktop/setup.zip") {
  
  files <- list.files(path_IQDesktop, all.files = TRUE, recursive = TRUE, full.names = FALSE)
  files <- grep("id_rsa", files, invert = TRUE, value = TRUE)
  
  unlink(filename_zip)
  curwd <- getwd()
  setwd(path_IQDesktop)
  zip(zipfile = filename_zip, files)
  setwd(curwd)
  
}
