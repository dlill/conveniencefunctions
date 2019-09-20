
#' to be completed
toggle_subsection <- function() {
  e <- rstudioapi::getSourceEditorContext()
  f <- readLines(e$path)
  subsection_lines <- which(stringr::str_detect(f, "[^-]-----$"))
  
  # .. 0 -----
  # .. 1 -----
  # .. 2 ----
  # .. 3 ------
  
  for (sl in subsection_lines)
    rstudioapi::insertText(rstudioapi::document_position(sl, -1), "#")
  
}
