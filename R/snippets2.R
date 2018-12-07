
#' Print a header to your console
#'
#' @export
#'
insert_header <- function(){
  paste0("rm(list = ls())",                                             "\n",
         "library(conveniencefunctions)",                               "\n",
         "devtools::load_all('~/PROJTOOLS/IQRtoolsGITHUB/IQRtools/')",  "\n",
         "development_path <- '~/PROJTOOLS/IQRtoolsGITHUB/99_Development_Environment/QSP/sysdMod-files/active-development'", "\n",
         "sysdMod_files <- list.files(development_path, pattern = 'syspharm')", "\n",
         "walk(sysdMod_files, ~ source(file.path(development_path,.x)))",  "\n",
         "#library(IQRtools)",                                            "\n",
         "currentwd <- '", getwd(),                                      "'\n",
         "projwd <- '", rstudioapi::getActiveProject(),                  "'\n",
         "scriptwd <- '", dirname(rstudioapi::getActiveDocumentContext()$path), "'\n",
         "setwd(scriptwd)", "\n",
         "load('workspace.rda')","\n") %>% cat
  invisible(NULL)
}


#' Print a text into the script
#'
#' @param string Character 1L. if longer, will be collapsed by ", "
#' @param wrap_in_quotes wrap the string into quotes
#' @param assignment character assign the string to a variable
#'
#' @export
#'
to_script <- function(string, wrap_in_quotes = F, assignment = NULL) {
  id <- rstudioapi::getSourceEditorContext()$id

  if (wrap_in_quotes)
    string <- paste0('"', string, '"')
  string <- paste0(string, collapse = ", ")
  if (!is.null(assignment))
    paste0(assignment, " <- ", string)

  rstudioapi::insertText(text = string, id = id)
}



# snippet section
# # ---------------------------------------------------------- #
# # ${1:title} ----
# # ---------------------------------------------------------- #
#
# snippet subsection
# # ----------------------------------------------- #
# # ${1:title} ----
# # ----------------------------------------------- #
#
# snippet nsubsection
# # ----------------------------------------------- #
# # .. ${1:title} ----
# # ----------------------------------------------- #
#
# snippet nsubsubsection
# # ------------------------------------- #
# # .... ${1:title} ----
# # ------------------------------------- #
