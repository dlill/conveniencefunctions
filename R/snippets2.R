
#' Print a header to your console
#'
#' @export
#'
insert_header <- function(){
  header <- paste0(
    "# ---------------------------------------------------------- #", "\n",
    "# Purpose ----",                                                   "\n",
    "# ---------------------------------------------------------- #", "\n",
    "# ", "\n",
    "# ", "\n",
    "# ", "\n",
    "# ", "\n",
    "# ", "\n",
    "# ", "\n",
    "# ", "\n",
    "\n",
    "\n",
    "# ---------------------------------------------------------- #", "\n",
    "# Header ----",                                                   "\n",
    "# ---------------------------------------------------------- #", "\n",
    "rm(list = ls())",                                             "\n",
    "library(conveniencefunctions)",                               "\n",
    "currentwd <- '", getwd(),                                      "'\n",
    "projwd <- '", rstudioapi::getActiveProject(),                  "'\n",
    "scriptwd <- '", dirname(rstudioapi::getActiveDocumentContext()$path), "'\n",
    "setwd(scriptwd)", "\n",
    "load('workspace.rda')","\n")
  header %>% cat
  return(invisible(header))
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
