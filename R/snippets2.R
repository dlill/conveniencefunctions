
#' Print a header to your console
#'
#' @export
#'
insert_header <- function(insert_in_script = T){
  header <- paste0(
    "# ---------------------------------------------------------- #", "\n",
    "# Purpose ----",                                                   "\n",
    "# ---------------------------------------------------------- #", "\n",
    "# ", "\n",
    "# ", "\n",
    "# ", "\n",
    "\n",
    "# ---------------------------------------------------------- #", "\n",
    "# Dependencies (Scripts) ----",                                 "\n",
    "# ---------------------------------------------------------- #", "\n",
    "# ", "\n",
    "# ", "\n",
    "# ", "\n",
    "\n",
    "# ---------------------------------------------------------- #", "\n",
    "# Header ----",                                                   "\n",
    "# ---------------------------------------------------------- #", "\n",
    "rm(list = ls())",                                             "\n",
    "library(conveniencefunctions)",                               "\n",
    "setwd(here())", "\n",
    "\n",
    '#source(here("Scripts/S00 Auxiliaries.R"))', "\n",
    "fast <- TRUE", "\n",
    "\n",
    'outdir <- "', str_extract(basename(rstudioapi::getActiveDocumentContext()$path), "S[0-9]*"), '_outputs"', "\n",
    "if (!dir.exists(here('Outputs', outdir)))", "\n",
    "  dir.create(here('Outputs', outdir))", "\n",
    "\n",
    '.base_name <- "plots_', str_extract(basename(rstudioapi::getActiveDocumentContext()$path), "S[0-9]*"), '"', "\n",
    "next_file(purge = TRUE)", "\n"
    )
  if (insert_in_script)
    rstudioapi::insertText(header)
  return(invisible(header))
}


#' @export
#' @rdname insert_header
insert_exit <- function(insert_in_script = T){
  exit <- paste0(
    "# ---------------------------------------------------------- #", "\n",
    "# Exit ----",                                                   "\n",
    "# ---------------------------------------------------------- #", "\n",
    'setwd(here("Outputs", outdir))', "\n",
    '# save.image("workspace_', str_extract(basename(rstudioapi::getActiveDocumentContext()$path), "S[0-9]*"), '.rda")', "\n",
    "unlink_dMod()" , "\n",
    "while(dev.cur() > 1){", "\n",
    "  dev.off()", "\n",
    "}", "\n",
    "\n",
    "\n",
    "\n",
    "\n",
    "\n",
    "\n"

  )
  if (insert_in_script)
    rstudioapi::insertText(exit)
  return(invisible(exit))
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
