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


#' @export
#' @rdname toggle
transform_subsection <- function(line, text, editor) {
  e <- rstudioapi::getSourceEditorContext()
  line <- e$selection[[1]]$range$start[[1]]
  text <- readLines(e$path)
  linetext <- text[line]
  if (grepl(" -----$", linetext)) {
    rstudioapi::insertText(c(line, Inf), "-")
    rstudioapi::insertText(c(line, 3), "..")
  } else if (grepl(" ------$", linetext)) {
    rstudioapi::modifyRange(c(line, nchar(linetext), line, Inf), "")
    rstudioapi::modifyRange(c(line, 1, line, 7), "# ..")
  }
  rstudioapi::documentSave(e$id)
  NULL
}

#' @export
#' @rdname toggle
initiate_or_delete_subsection <- function(line, text, editor) {
  e <- rstudioapi::getSourceEditorContext()
  line <- e$selection[[1]]$range$start[[1]]
  text <- readLines(e$path)
  linetext <- text[line]
  if (grepl(" -{6}$", linetext)) {
    # Remove
    rstudioapi::modifyRange(c(line, nchar(linetext)-5, line, Inf), "")
    rstudioapi::modifyRange(c(line, 1, line, 8), "")
  } else if (grepl(" -{5}$", linetext)) {
    # Remove
    rstudioapi::modifyRange(c(line, nchar(linetext)-4, line, Inf), "")
    rstudioapi::modifyRange(c(line, 1, line, 6), "")
  } else if (grepl("^# ", linetext)) {
    # Turn comment into subsection
    rstudioapi::insertText(c(line, 3), ".. ")
    rstudioapi::insertText(c(line, Inf), "-----")
  } else {
    # Turn code into subsection
    rstudioapi::insertText(c(line, 1), "# .. ")
    rstudioapi::insertText(c(line, Inf), "-----")
  }
  rstudioapi::documentSave(e$id)
  NULL
}

