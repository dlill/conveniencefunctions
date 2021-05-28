# -------------------------------------------------------------------------#
# Subsection ----
# -------------------------------------------------------------------------#

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
# Loopdebugger ----
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
  loopval <- trimws(gsub(".*apply\\((.+), *function.*", "\\1", textline))
  loopvar <- gsub(".*apply\\(.+, *function\\( *(\\w+) *\\).*", "\\1", textline)
  list(loopvar = loopvar, loopval = loopval)
}

#' Get the arguments of a for loop or of lapply
#'
#' @param textline Line of code
#'
#' @return list(loopvar = "loopingvariable", loopval = "list:ofValues")
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



# -------------------------------------------------------------------------#
# Function arguments ----
# -------------------------------------------------------------------------#

#' Extract arguments from a function call
#'
#' @return
#' @export
insert_arguments <- function() {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  
  # Allow one-line statements with cursor or selections
  if (identical(e$selection[[1]]$range$start,e$selection[[1]]$range$end)){
    text <- readLines(e$path)[e$selection[[1]]$range$start[1]]
  } else {
  text <- paste0(e$selection[[1]]$text, collapse = " ")
  }
  text <- gsub("\\n", "", text)
  text <- gsub(" +", " ", text)
  text <- trimws(text)
  # handle function assignments
  if (grepl("function", text)) 
    text <- gsub(" *(<-|=) *function", "", text)
  # handle assignments
  text <- gsub("(.*) *<- *", "", text)
  
  funname <- gsub("(\\w+)\\(.*", "\\1", text)
  
  text1 <- substr(text, nchar(funname)+ 2, nchar(text)-1)
  text1 <- strsplit(text1, "=")[[1]]
  text1 <- trimws(text1)
  argnames1 <- unlist(regmatches(text1[1:(length(text1)-1)], gregexpr("(\\w|\\.)+$", text1[1:(length(text1) - 1)])))
  args1 <- gsub("(.*), *(\\w|\\.)+$", "\\1", text1[2:length(text1)])
  
  call <- paste0(argnames1, " = ", args1)
  
  rstudioapi::insertText(
    location = rstudioapi::as.document_position(c(e$selection[[1]]$range[["start"]][[1]],1)),
    paste0(paste0(call, collapse = "\n"), "\n"),
    id = e$id)
  rstudioapi::documentSave(id = e$id)
  bla <- NULL
}

# -------------------------------------------------------------------------#
# Toggle mclapply/lapply ----
# -------------------------------------------------------------------------#

#' Title
#'
#' @param textline 
#'
#' @return
#' @export
#'
#' @examples
#' textline <- "fuck <- lapply(1:n, function(x) {"
#' toggle_mclapply_on(textline)
#' textline <- "fuck <- lapply(motherfuck[1:3], function(x) bla::fn(x))"
#' toggle_mclapply_on(textline)
#' 
toggle_mclapply_on <- function(textline) {
  loopargs <- extract_apply(textline = textline)
  newline <- textline
  newline <- paste0("ncores <- 4\n", newline)
  newline <- gsub("lapply", "parallel::mclapply", newline)
  newline <- gsub(loopargs$loopval, paste0("X = ", loopargs$loopval, ", mc.cores = ncores"), newline, fixed = TRUE)
  newline <- gsub("function", "FUN = function", newline)
  newline
}

#' Title
#'
#' @param textline 
#'
#' @return
#' @export
#'
#' @examples
#' textline <- "fuck <- mclapply(X = 1:n, mc.cores = ncores, FUN = function(x) {"
#' toggle_mclapply_off(textline)
toggle_mclapply_off <- function(textline) {
  newline <- textline
  newline <- gsub("parallel::mclapply", "lapply", newline)
  newline <- gsub("X = ", "", newline)
  newline <- gsub(", mc.cores = ncores", "", newline)
  newline <- gsub("FUN = ", "", newline)
  newline
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
toggle_mclapply <- function() {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  current_row <- e$selection[[1]]$range$start[1]
  text <- readLines(e$path)
  textline <- text[current_row]
  
  rng <- newline <- NULL
  if (grepl("mclapply", textline)) {
    newline <- toggle_mclapply_off(textline)
    del_line_above <- 0
    if (grepl("^ncores *<-", text[current_row-1])) del_line_above <- 1
    rng <- rstudioapi::document_range(
      rstudioapi::document_position(current_row - del_line_above, 1), 
      rstudioapi::document_position(current_row, Inf))
  } else {
    newline <- toggle_mclapply_on(textline)
    rng <- rstudioapi::document_range(
      rstudioapi::document_position(current_row, 1), 
      rstudioapi::document_position(current_row, Inf))
  }
  
  rstudioapi::modifyRange(location = rng, text = newline, id = e$id)
  rstudioapi::documentSave(id = e$id)
  
  sink <- NULL
}



# -------------------------------------------------------------------------#
# Debugonce ----
# -------------------------------------------------------------------------#

#' Guess function name of interest
#'
#' @param textline character(1L), line of script with buggy function
#'
#' @return extracted function name
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
#'
#' @examples
#' textline <- "rng <- rstudioapi::document_range("
#' textline <- "rstudioapi::document_range("
#' textline <- "document_range(kjshdf = bla)"
guess_function <- function(textline) {
  gsub("(.*<- *)?(\\w+:+)?(\\w+)\\(.*", "\\2\\3", textline)
}

#' Insert debugonce(function)
#' 
#' rng <- rstudioapi::document_range(...)
#' 
#' gets turned into
#' 
#' debugonce(rstudioapi::document_range)
#' rng <- rstudioapi::document_range(...)
#' 
#' @return NULL. Modifies the document
#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
#' @export
insert_debugonce <- function() {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  current_row <- e$selection[[1]]$range$start[1]
  text <- readLines(e$path)
  textline <- text[current_row]
  
  newline <- paste0("debugonce(", guess_function(textline), ")\n")
  rstudioapi::insertText(location = rstudioapi::document_position(current_row, 1), newline, id = e$id)
  rstudioapi::documentSave(id = e$id)
  
  sink <- NULL
}


# -------------------------------------------------------------------------#
# BLABLA ----
# -------------------------------------------------------------------------#

#' Toggle BLABLA so the documentwalker can extract functions
#' 
#' @details rstudioapi getSourceEditorContext documentSave setDocumentContents 
#' 
#' @return NULL: Modifies document
#' @export
toggle_blabla <- function() {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  text <- readLines(e$path)
  
  if (!any(grepl("BLABLA", text))){
    text <- gsub("[,", "[BLABLA,", text, fixed = TRUE)
    text <- gsub(",]", ",BLABLA]", text, fixed = TRUE)
  } else {
    text <- gsub("BLABLA", "", text, fixed = TRUE)
  }
  
  text <- paste0(text, collapse = "\n")
  text <- paste0(text, "\n")
  
  rstudioapi::setDocumentContents(text, id = e$id)
  rstudioapi::documentSave(id = e$id)
}

# -------------------------------------------------------------------------#
# Toggle ggplot ----
# -------------------------------------------------------------------------#

# idea turn ggplots into one-liners and back to multiliners

# pl <- cfggplot(dplot1[!Experiment %in% c("81", "81.1")], aes(time, FC)) + 
#   facet_grid(name ~ cellline + TGFb, scales = "free") + 
#   geom_line(aes(color = factor(TGFb), linetype = factor(Experiment), group = interaction(Experiment, cellline, TGFb))) + 
#   geom_hline(yintercept = 0) + 
#   scale_color_viridis_d() +
#   scale_y_log10() + 
#   labs(title = "FC - Genes normalized wrt housekeeper, technical replicates reduced to mean, FC wrt time = 0", 
#        subtitle = "Experiments show systematic effect: Strong impact of time 0",
#        x = "Time (hours)", lty = "Experiment") + 
#   geom_blank()
# 
# cf_outputFigure(pl, filename = file.path(.outputFolder, "015-proteins-fc-doseresponse-ExperimentsMingled-no81-perTGFb.png"), width = 29.7, height = 21, scale = 1, units = "cm")
# 
# e <- rstudioapi::getSourceEditorContext()
# rstudioapi::documentSave(id = e$id)
# text <- readLines(e$path)
# ln <- grep("(facet_|geom_|scale_|labs|theme)", text)
# 
# paste0(text[ln], collapse = "")
# 
# 

# -------------------------------------------------------------------------#
# importFrom ----
# -------------------------------------------------------------------------#

#' Title
#'
#' @return
#' @export
#'
#' @details rstudioapi getSourceEditorContext documentSave
#' 
#' @importFrom stringr str_extract_all
#' @importFrom data.table as.data.table rbindlist setnames
#'
#' @examples
extract_importFrom <- function() {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  
  text <- e$selection[[1]]$text
  text <- strsplit(text, "\n", fixed = TRUE)[[1]]
  text <- stringr::str_extract_all(text, "[a-zA-Z._]++::[a-zA-Z._]+")
  text <- unlist(text)
  text <- strsplit(text, "::", TRUE)
  text <- lapply(text, function(x) data.table::as.data.table(as.list(x)))
  text <- data.table::rbindlist(text)
  if (length(text)){
  data.table::setnames(text, c("pkg", "fun"))
  text <- text[,list(text = paste0("#' @importFrom ", unique(pkg), " ", paste0(unique(fun), collapse = " "))), by = "pkg"]
  text <- paste0(paste0(text$text, "\n"), collapse = "")
  } else text <- NULL
  
  # Add other stuff as well (hacky but can be cleaned in a breeze, just remove this row)
  text <- paste0("#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)\n",
                 "#' @md\n", 
                 "#' @family \n", 
                 text)
  
  # Insert into beginning of selection (preferably select up to @export)
  position_toInsert <- rstudioapi::as.document_position(c(e$selection[[1]]$range$start[1], 1))
  rstudioapi::insertText(location = position_toInsert,text = text, id = e$id)
  
  invisible(text)
}





# -------------------------------------------------------------------------#
# Function call ----
# -------------------------------------------------------------------------#

#' Title
#'
#' @return
#' @export
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#'
#' @examples
refactor_functionCall <- function() {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  current_row <- e$selection[[1]]$range$start[1]
  text <- readLines(e$path)
  textline <- text[current_row]
  nchar_current_row <- nchar(textline)
  textline <- gsub(" ?<- ?function| ?\\{", "", textline)
  
  # Insert into beginning of selection (preferably select up to @export)
  rng <- rstudioapi::document_range(rstudioapi::document_position(current_row,1), 
                                    rstudioapi::document_position(current_row,nchar_current_row+1))
  rstudioapi::modifyRange(location = rng, text= textline, id = e$id)
  
  invisible(text)
}


