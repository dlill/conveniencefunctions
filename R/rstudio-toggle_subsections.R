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

#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
#' @export
#' @rdname toggle
initiate_or_delete_subsection <- function(line, text, editor) {
  e <- rstudioapi::getSourceEditorContext()
  line <- e$selection[[1]]$range$start[[1]]
  text <- readLines(e$path)
  linetext <- text[line]
  if (grepl(" -{6}$", linetext)) {
    # Remove
    rstudioapi::modifyRange(c(line, nchar(linetext)-6, line, Inf), "")
    rstudioapi::modifyRange(c(line, 1, line, 8), "")
  } else if (grepl(" -{5}$", linetext)) {
    # Remove
    rstudioapi::modifyRange(c(line, nchar(linetext)-5, line, Inf), "")
    rstudioapi::modifyRange(c(line, 1, line, 6), "")
  } else if (grepl("^# ", linetext)) {
    # Turn comment into subsection
    rstudioapi::insertText(c(line, 3), ".. ")
    rstudioapi::insertText(c(line, Inf), " -----")
  } else {
    # Turn code into subsection
    rstudioapi::insertText(c(line, 1), "# .. ")
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
  text <- readLines(e$path)
  
  # .. 1 Get lines -----
  s1all <- s1 <- grep("(?<!Exit) -{4}$", text, perl = TRUE)
  if (FLAGfunctionAsSection){
    functions <- grep("function(", text, fixed = TRUE)
    s1all <- c(s1all, functions)
  }
  s2 <- grep(" -{5}$", text)
  s3 <- grep(" -{6}$", text)
  
  # .. 1  # Associate subsubs to subs -----
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
  # .. 2  # Associate subs to s -----
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
  
  # .. 3 Number sections -----
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
  NULL
}





