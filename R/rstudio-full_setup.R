#' Quick setup of RStudio on a new machine
#'
#' 
#' @return called for side-effect
#' @export
cf_install_rstudio <- function(FLAGoverwrite = FALSE, IQdesktopVersion = c("local", "Ueli")) {
  # 1. Theme 
  rstudioapi::applyTheme("pastel on dark")
  # 2. Keybindings
  install_cfkeybindings(FLAGoverwrite = FLAGoverwrite)
  # 3. Snippets
<<<<<<< HEAD
  install_cfsnippets(FLAGoverwrite = FLAGoverwrite)  
=======
  install_cfsnippets(FLAGoverwrite = TRUE)  
>>>>>>> 6cef049190368b69da505c761c98fbd1f0df07d3
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
  if (!dir.exists("~/.config/gtk-3.0")) dir.create("~/.config/gtk-3.0")
  thunarfile <- switch(IQdesktopVersion[1], "local" = "setup_IQDesktop/thunar_shortcuts/bookmarks", "Ueli" = "setup_IQDesktop/thunar_shortcuts/bookmarksUeli")
  wup <- file.copy(system.file(thunarfile, package = "conveniencefunctions"), "~/.config/gtk-3.0/bookmarks", overwrite = FLAGoverwrite) 
  if (wup) cat("Explorer shortcuts installed \n")
  # # 6. IQRmate
  # devtools::install_github("IntiQuan/IQRtools", subdir = "IQRmate")
  cat("cd ", "mkdir PROJTOOLS",  "cd PROJTOOLS", 
      "git clone git@github.com:dlill/conveniencefunctions",
      "",
      "git clone git@github.com:IntiQuan/iqrmalaria IQRmalariaGIT",
      "git clone git@github.com:IntiQuan/MMVIsoboles",
      "",
      "cd ~/PROJECTS",
      "gclone /IQDESKTOP/SHARE/ (drop final /)",
      "",
      "setup_IQRtools(local = TRUE)",
      '.RESERVED_WORD_IQRMODELS                 <- c( 
  "T","F","PK","G","H","gt","ge","lt","le","mod","and","or", 
  "piecewise","interp0","interp1","interpcs", "default", "F1", "F2", "Tlag", "eps", "eta", "theta", "sigma", 
  "a","b","b1","b2","b3","d","Intercept", 
  "time", "y", "ydot", "RPAR", "IPAR" 
)',
      "install.packages('akima')",
<<<<<<< HEAD
=======
      "",
      "sudo apt-get install  libx11-dev mesa-common-dev libglu1-mesa-dev",
      "install.packages('kmlShape')",
>>>>>>> 6cef049190368b69da505c761c98fbd1f0df07d3
      sep = "\n"
      )  
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