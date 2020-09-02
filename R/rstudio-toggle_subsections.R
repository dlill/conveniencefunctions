#' @rdname toggle
#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
toggle_off <- function(line, text, editor) {
  d1 <- rstudioapi::document_position(line, nchar(text[line]) + 1)
  rstudioapi::modifyRange(d1, " #", editor$id)
  NULL
}

#' @rdname toggle
#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
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
#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
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

#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
#' @export
#' @rdname toggle
toggle_section <- function() toggle("----")

#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
#' @export
#' @rdname toggle
toggle_subsection <- function() toggle("-----")

#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
#' @rdname toggle
#' @export
toggle_subsubsection <- function() toggle("------")


#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
#' @export
#' @rdname toggle
transform_subsection <- function(line, text, editor) {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  line <- e$selection[[1]]$range$start[[1]]
  text <- readLines(e$path)
  linetext <- text[line]
  ws <- nchar(regmatches(linetext, regexpr(" *", linetext)))
  if (grepl(" -----$", linetext)) {
    rstudioapi::insertText(c(line, Inf), "-")
    rstudioapi::insertText(c(line, ws+3), "..")
  } else if (grepl(" ------$", linetext)) {
    rstudioapi::modifyRange(c(line, nchar(linetext), line, Inf), "")
    rstudioapi::modifyRange(c(line, ws+1, line, ws+7), "# ..")
  }
  rstudioapi::documentSave(e$id)
  NULL
}

#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
#' @export
#' @rdname toggle
initiate_or_delete_subsection <- function(line, text, editor) {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  line <- e$selection[[1]]$range$start[[1]]
  text <- readLines(e$path)
  linetext <- text[line]
  ws <- nchar(regmatches(linetext, regexpr(" *", linetext)))
  if (grepl(" -{6}$", linetext)) {
    # Remove
    rstudioapi::modifyRange(c(line, nchar(linetext)-6, line, Inf), "")
    rstudioapi::modifyRange(c(line, ws+1, line, ws+8), "")
  } else if (grepl(" -{5}$", linetext)) {
    # Remove
    rstudioapi::modifyRange(c(line, nchar(linetext)-5, line, Inf), "")
    rstudioapi::modifyRange(c(line, ws+1, line, ws+6), "")
  } else if (grepl("^ *# ", linetext)) {
    # Turn comment into subsection
    rstudioapi::insertText(c(line, ws+3), ".. ")
    rstudioapi::insertText(c(line, Inf), " -----")
  } else {
    # Turn code into subsection
    rstudioapi::insertText(c(line, ws+1), "# .. ")
    rstudioapi::insertText(c(line, Inf), " -----")
  }
  rstudioapi::documentSave(e$id)
  NULL
}


#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
#' @param FLAGfunctionAsSection Start subsections within functions with 1. Switch off if the function is just a small part of a bigger logic
#' @export
#' @rdname toggle
#' @importFrom data.table data.table
renumber_sections <- function(FLAGfunctionAsSection = FALSE) {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  text <- readLines(e$path)
  
  # .. 1 Get lines -----
  s1all <- s1 <- grep("(?<!Exit) -{4}$", text, perl = TRUE)
  if (FLAGfunctionAsSection){
    functions <- grep("function(", text, fixed = TRUE)
    s1all <- c(s1all, functions)
  }
  s2 <- grep(" -{5}$", text)
  s3 <- grep(" -{6}$", text)
  
  # .. 2  # Associate subsubs to subs -----
  if (length(s3)){
    ds3 <- data.table::data.table(s = s3)
    ds3[,`:=`(s2associated = which.min(s > s2) ), by = (1:nrow(ds3))]
    ds3[,`:=`(number = 1:.N), by = s2associated]
    for (i in 1:nrow(ds3)) {
      line <- ds3[i,s]
      n <- ds3[i,number]
      reg <- regexpr("# .... \\d* ?", text[line])
      rstudioapi::modifyRange(c(line, reg, line, reg + attr(reg, "match.length")), paste0("# .... ", n, " "))
    }
  }
  # .. 3  # Associate subs to s -----
  if (length(s2)){
    ds2 <- data.table::data.table(s = s2)
    ds2[,`:=`(s1associated = which.min(s > s1all) ), by = (1:nrow(ds2))]
    ds2[,`:=`(number = 1:.N), by = s1associated]
    for (i in 1:nrow(ds2)) {
      line <- ds2[i,s]
      n <- ds2[i,number]
      reg <- regexpr("# .. \\d* ?", text[line])
      rstudioapi::modifyRange(c(line, reg, line, reg + attr(reg, "match.length")), paste0("# .. ", n, " "))
    }
  }
  
  # .. 4 Number sections -----
  if (length(s1)){
    ds1 <- data.table::data.table(s = s1)
    ds1[,`:=`(number = 1:.N - 1)]  
    for (i in 1:nrow(ds1)) {
      line <- ds1[i,s]
      n <- ds1[i,number]
      reg <- regexpr("# \\d* ?", text[line])
      rstudioapi::modifyRange(c(line, reg, line, reg + attr(reg, "match.length")), paste0("# ", n, " "))
    }
  }
  rstudioapi::documentSave(id = e$id)
  NULL
}


# -------------------------------------------------------------------------#
# 0 Loopdebugger ----
# -------------------------------------------------------------------------#

#' @rdname extract_loopargs
#' @export
extract_for <- function(textline) {
  loopvar <- gsub(".*for ?\\((.+) in.*", "\\1", textline)
  loopval <- gsub(".*for ?\\(.+ in (.+)\\) ?\\{?.*", "\\1", textline)
  list(loopvar = loopvar, loopval = loopval)
}

#' @rdname extract_loopargs
#' @export
extract_apply <- function(textline) {
  loopval <- trimws(gsub(".*apply\\((.+),.*", "\\1", textline))
  loopvar <- gsub(".*apply\\(.+, ?function\\( *(.+) *\\).*", "\\1", textline)
  list(loopvar = loopvar, loopval = loopval)
}

#' Get the arguments of a for loop or of lapply
#'
#' @param textline Line of code
#'
#' @return list(looparg = "loopingvariable", loopval = "list:ofValues")
#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
#' @export
#'
#' @examples
#' extract_loopargs("lapply(names(alpha), function(x) 1)")
extract_loopargs <- function(textline) {
  if (grepl("apply\\(", textline)) return(extract_apply(textline))
  if (grepl("for ?\\(", textline)) return(extract_for(textline))
}

#' Insert the arguments of a loop into the script
#' 
#' for (a in 1:3) gets turned into
#' 
#' a <- (1:3)[[1]]
#' for (a in 1:3)
#' 
#' This is handy for developing and debugging a loop
#' 
#' @return NULL. Modifies the document
#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
#' @export
insert_loopdebugger <- function() {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  current_row <- e$selection[[1]]$range$start[1]
  text <- readLines(e$path)
  textline <- text[current_row]
  loopargs <- extract_loopargs(textline)
  
  newline <-   paste0(loopargs$loopvar, " <- (", loopargs$loopval, ")[[1]]\n")
  
  rstudioapi::insertText(location = rstudioapi::document_position(current_row, 1), newline, id = e$id)
  rstudioapi::documentSave(id = e$id)
  
  sink <- NULL
  
}


#' Extract arguments from a function call
#'
#' @return
#' @export
#'
#' @examples
insert_arguments <- function() {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  
  # Allow one-line statements with cursor or selections
  if (identical(e$selection[[1]]$range$start,e$selection[[1]]$range$start)){
    text <- readLines(e$path)[e$selection[[1]]$range$start[1]]
  } else {
  text <- paste0(e$selection[[1]]$text, collapse = " ")
  }
  text <- gsub("\\n", "", text)
  text <- gsub(" +", " ", text)
  # handle function assignments
  if (grepl("function", text)) 
    text <- gsub(" *(<-|=) *function", "", text)
  # handle assignments
  text <- gsub("(.*) *<- *", "", text)
  
  funname <- gsub("(\\w+)\\(.*", "\\1", text)
  r1 <- r0 <- regmatches(text, regexec("\\(([^()]|(?R))*\\)", text, perl = TRUE))[[1]]
  r1 <- setNames(r1, paste0("XXXX", seq_along(r0)))
  r <- r1[1]
  if (length(r1) > 1)
    for (i  in 2:length(r1)){
      r <- gsub(r1[i], names(r1)[i], r, fixed = TRUE)
      }
  r <- gsub("\\(|\\)", "", r)
  splits <- strsplit(r, ",")[[1]]
  d <- data.table(string = splits)
  d[,`:=`(pos = 1:.N)]
  d[,`:=`(formal = gsub("(\\w+) *=.*", "\\1", string))]
  d[grep("=", string, invert = TRUE),`:=`(formal =formalArgs(funname)[pos])]
  d[,`:=`(arg = gsub(paste0(formal, " *= *"), "", string)), by = 1:nrow(d)]
  d[,`:=`(arg = {
    if (length(r1) > 1)
      for (i  in 2:length(r1)){
        arg <- gsub(names(r1)[i], r1[i], arg, fixed = TRUE)
      }
    arg
  })]
  d[,`:=`(call = paste0(formal, " = ", arg))]
  
  rstudioapi::insertText(
    location = rstudioapi::as.document_position(c(e$selection[[1]]$range[["start"]][[1]],1)),
    paste0(paste0(d$call, collapse = "\n"), "\n"),
    id = e$id)
  rstudioapi::documentSave(id = e$id)
  bla <- NULL
}

