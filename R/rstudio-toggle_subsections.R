#' @rdname toggle
toggle_off <- function(line, text, editor) {
  d1 <- rstudioapi::document_position(line, nchar(text[line]) + 1)
  rstudioapi::modifyRange(d1, " #", editor$id)
  NULL
}

#' @rdname toggle
toggle_on <- function(line, text, editor) {
  d1 <- rstudioapi::document_position(line, nchar(text[line]) - 1)
  d2 <- rstudioapi::document_position(line, nchar(text[line]) + 1)
  r <- rstudioapi::document_range(d1,d2)
  rstudioapi::modifyRange(r, "", editor$id)
  NULL
}

#' Toggle sections
#'
#' In the document outline, toggle a section on or off by adding or removing a #
#'
#' @return called for side-effect
toggle <- function(dashes = "----") {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(e$id)
  e <- rstudioapi::getSourceEditorContext()
  text <- readLines(e$path)

  sec_on    <- grep(paste0("# (.*) ", dashes, "$"), text)
  sec_off   <- grep(paste0("# (.*) ", dashes, " #$"), text)

  if (length(sec_off)){
    lapply(sec_off, toggle_on, text = text, editor = e)
    return(NULL)
  } else {
    lapply(sec_on, toggle_off, text = text, editor = e)
    return(NULL)
  }
}

#' @export
#' @rdname toggle
toggle_section <- function() toggle("----")

#' @export
#' @rdname toggle
toggle_subsection <- function() toggle("-----")

#' @rdname toggle
#' @export
toggle_subsubsection <- function() toggle("------")


