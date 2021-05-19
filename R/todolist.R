#' Print a todolist to your console or to a file
#'
#' Collect all TODO points in the scripts of a project and write them into TODO.md
#'
#' @param path path to directory to grep in
#' @param FLAGpipes Boolean. search for pipe operators?
#' @param filename If provided, write the todolist to this file (recommended ending: *.md)
#'
#' @export
#'
#' @importFrom dplyr mutate arrange case_when
#' @importFrom stringr str_detect str_replace_all str_extract fixed
#'
todolist <- function(path = getwd(), FLAGpipes = FALSE, filename = NULL) {

  # * Wrap in function so errors can be caught and the working directory can be reset
  assemble_todolist <- function(FLAGpipes) {
    # .. Todolist ----#
    grepout <- system('grep -I -r "\\\\[[x ]*\\\\]"', intern = TRUE)
    # files <- list.files(path, "\\.(R|md|txt)", recursive = T, full.names = TRUE)
    # todo_frame <- lapply(setNames(files, basename(files)), function(f) {
    #   ft <- grep("\\[[x ]*\\]", readLines(f), value = TRUE)
    #   todo_frame <- data.table(gout = ft)
    # })
    todo_frame <- data.frame(gout = grepout, stringsAsFactors = FALSE)
    todo_frame <- mutate(todo_frame,
                         done = str_detect(gout, fixed("[x]")),
                         script = str_extract(gout, "^.*\\.R"),
                         script = case_when(!is.na(str_extract(gout, "^S\\d+")) ~ str_extract(gout, "^S\\d+"),
                                            TRUE ~ script),
                         scriptn = as.numeric(str_extract(script, "\\d.*")),
                         task = str_replace_all(gout, "^.*\\[ *\\] (.*)$", "\\1"),
                         checkbox = str_extract(gout, "\\[[x ]*\\]")
    )
    todo_frame <- arrange(todo_frame, done, scriptn)
    todo_frame <- mutate(todo_frame, todolist = paste0(checkbox, " ", script, ": ", task))

    mysplit <- split(todo_frame, todo_frame$done)
    out <- c(
      "TODO", "", mysplit[["FALSE"]]$todolist, "", "",
      "DONE", "", mysplit[["TRUE"]]$todolist, "", ""
    )

    # .. Pipe-operators ----#
    if (FLAGpipes){
      grepout <- system('grep -r "%>%"', intern = TRUE)
      pipe_frame <- tibble(gout = grepout)
      pipe_frame <- mutate(pipe_frame,
                           done = "PIPE",
                           script = str_extract(gout, "^S\\d+"),
                           scriptn= as.numeric(str_extract(script, "\\d.*")),
                           task = str_replace_all(gout, "^(.*)%>%.*$", "\\1"),
                           checkbox = str_extract(gout, "%>%")
      )
      pipe_frame <- arrange(pipe_frame, done, scriptn)
      pipe_frame <- mutate(pipe_frame, todolist = paste0(checkbox, " ", script, ": ", task))
      out <- c(out, "PIPE-OPERATORS", "", pipe_frame$todolist, "", "")
    }

    return(out)
  }


  # .. Main body----#
  oldwd <- getwd()
  setwd(path)
  out <- tryCatch(assemble_todolist(FLAGpipes), error = function(e) e)
  setwd(oldwd)

  # .. cat ----#
  cat(paste0(out, collapse = "\n"))
  # .. write ----#
  if (!is.null(filename))
    writeLines(out, filename)

  # .. return ----#
  return(invisible(out))
}

