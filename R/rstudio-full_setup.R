#' Quick setup of RStudio on a new machine
#'
#' 
#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
#' @return called for side-effect
#' @export
cf_install_rstudio <- function(FLAGoverwrite = TRUE, 
                               FLAGThunarShortcuts = FALSE,
                               FLAGIQRreservedWords = FALSE) {
  # 1. Theme 
  try(rstudioapi::applyTheme("pastel on dark"))
  # 2. Keybindings
  install_cfkeybindings(FLAGoverwrite = FLAGoverwrite)
  # 3. Snippets
  install_cfsnippets(FLAGoverwrite = FLAGoverwrite)  
  # 4. Bash alias for git
  bashrcfile <- "~/.bashrc"
  if (Sys.info()["sysname"] == "Windows") bashrcfile <- "~/../.bashrc"
  newLines <- readLines(system.file("setup_IQDesktop/Setup/Resources/bash/bash_aliases", package = "conveniencefunctions")) 
  if (!any(grepl("# ===== conveniencefunctions aliases =======", readLines(bashrcfile))))
    cat(c("\n", newLines), sep = "\n", file = bashrcfile, append = TRUE)
  # 5. Install shortcuts for Thunar
  if (FLAGThunarShortcuts) install_thunarshortcuts(FLAGThunarShortcuts)
  # 6. .Rprofile
  if (FLAGoverwrite) file.copy(system.file("setup_IQDesktop/Setup/Resources/.Rprofile"), "~/.Rprofile")
  # 7. 
  if (FLAGIQRreservedWords) install_cfreservedWords()
}


#' Copy a standard setup_options_IQRtools.R for custom settings
#'
#' @return
#' @export
#'
#' @examples
install_cfreservedWords <- function(){
  file.copy(system.file("setup_IQDesktop/Setup/Resources/setup_options_IQRtools.R"), "~/setup_options_IQRtools.R")
}
#' Install thunar shortcuts
#'
#' @param FLAGoverwrite 
#'
#' @return
#' @export
#'
#' @examples
install_thunarshortcuts <- function(FLAGoverwrite) {
  if (!dir.exists("~/.config/gtk-3.0")) dir.create("~/.config/gtk-3.0")
  wup <- file.copy(system.file("setup_IQDesktop/Setup/Resources/thunar_shortcuts/bookmarks", package = "conveniencefunctions"), 
                   "~/.config/gtk-3.0/bookmarks", overwrite = FLAGoverwrite) 
  if (wup) cat("Explorer shortcuts installed \n")
}


#' Title
#'
#' @return
#' @export
#'
#' @examples
update_thunarshortcuts_in_cf <- function() {
  thunarfile_new <- "~/.config/gtk-3.0/bookmarks"
  thunarfile_old <- "~/PROJTOOLS/conveniencefunctions/inst/setup_IQDesktop/Setup/Resources/thunar_shortcuts/bookmarks"
  
  new <- NULL
  if (!file.exists(thunarfile_new)) stop("no new bookmarks found")
  new <- readLines(thunarfile_new)
  old <- readLines(thunarfile_old)
  
  cat(c("", setdiff(new, old), ""), sep = "\n", file = thunarfile_old, append = TRUE)
}



#' Install keybindings
#'
#' @export
#' @importFrom stats setNames
install_cfkeybindings <- function(FLAGoverwrite = FALSE){
  keybindings_path <- "~/.R/rstudio/keybindings"
  if (!dir.exists(keybindings_path)) dir.create(keybindings_path, FALSE, TRUE)
  keybindings_files <- list.files(system.file("setup_rstudio/keybindings", package = "conveniencefunctions"), "json$", F, T)
  wup <- vapply(stats::setNames(nm = keybindings_files), file.copy, to = keybindings_path, overwrite = FLAGoverwrite, FUN.VALUE = TRUE)
  if (any(wup)) cat(paste0(names(wup)[wup], collapse = " .... \n"),  "\nkeybindings installed\n")
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
  wup <- file.copy(system.file("setup_rstudio/snippets/r.snippets", package = "conveniencefunctions"), file.path("~/.R/snippets/r.snippets"), overwrite = FLAGoverwrite)
  if (wup) cat("Snippets were overwritten \n")
  NULL
}

