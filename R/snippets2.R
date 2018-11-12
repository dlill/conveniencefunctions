
#' Print a header to your console
#'
#' @export
#'
insert_header <- function(){
  paste0("rm(list = ls())",                                             "\n",
         "library(conveniencefunctions)",                               "\n",
         "devtools::load_all('~/PROJTOOLS/IQRtoolsGITHUB/IQRtools/')",  "\n",
         "development_path <- '~/PROJTOOLS/IQRtoolsGITHUB/99_Development_Environment/QSP/sysdMod-files/'", "\n",
         "sysdMod_files <- list.files(development_path, pattern = 'dMod')", "\n",
         "walk(sysdMod_files, ~ source(file.path(development_path,.x)))",  "\n",
         "#library(IQRtools)",                                            "\n",
         "currentwd <- '", getwd(),                                      "'\n",
         "projwd <- '", rstudioapi::getActiveProject(),                  "'\n",
         "scriptwd <- '", dirname(rstudioapi::getActiveDocumentContext()$path), "'\n",
         "setwd(scriptwd)", "\n",
         "load('workspace.rda')","\n") %>% cat
  invisible(NULL)
}


