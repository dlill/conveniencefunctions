# -------------------------------------------------------------------------#
# 1 dfgh ----
# -------------------------------------------------------------------------#

# -------------------------------------------------------------------------#
# 5 dfgh ----
# -------------------------------------------------------------------------#

# .. 1 sdf -----
# .... 1 sdfasfgah ------
# .... 2 sdfassdffgah ------
# .. 3 sdfasdf -----
# .... 1 sdfasfgah ------

# -------------------------------------------------------------------------#
# 4 jdsfg ----
# -------------------------------------------------------------------------#

# .. sdf -----
# .... 1 sdfasfgah ------
# .... 2 sdfasfgah ------
# .... 3 sdfasfdfggah ------

# .. sdfgsdf -----

# Exit ----

#' @export
#' @rdname toggle
renumberSections <- function(line, text, editor, FLAGfunctionAsSection = TRUE) {
  # >> Script development. # [] Remove in final script << ----
  FLAGfunctionAsSection = TRUE
  e <- rstudioapi::getSourceEditorContext()
  text <- readLines(e$path)
  
  # .. Get lines -----
  s1 <- grep("(?<!Exit) -{4}$", text, perl = TRUE)
  if (FLAGfunctionAsSection){
    functions <- grep("function(", text, fixed = TRUE)
    s1 <- c(s1, functions)}
  s2 <- grep(" -{5}$", text)
  s3 <- grep(" -{6}$", text)
  
  # ..   # Associate subsubs to subs -----
  ds3 <- data.table(s = s3)
  ds3[,`:=`(s2associated = which.min(s > s2) - 1), by = (1:nrow(ds3))]
  ds3[,`:=`(number = 1:.N), by = s2associated]
  
  # ..   # Associate subs to s -----
  ds2 <- data.table(s = s2)
  ds2[,`:=`(s1associated = which.min(s > s1) - 1), by = (1:nrow(ds2))]
  ds2[,`:=`(number = 1:.N), by = s1associated]
  
  # .. Number sections -----
  ds1 <- data.table(s = s1)
  ds1[,`:=`(number = 1:.N - 1)]  
  
  
  # .. do renaming -----
  i <- 1
  for (i in 1:nrow(ds3)) {
    line <- ds3[i,s]
    n <- ds3[i,number]
    reg <- regexpr("# .... \\d+", text[line])
    rstudioapi::modifyRange(c(line, reg, line, reg + attr(reg, "match.length")), paste0("# .... ", n))
    }
  
  for (i in 1:nrow(ds2)) {
    line <- ds2[i,s]
    n <- ds2[i,number]
    reg <- regexpr("# .. \\d+", text[line])
    rstudioapi::modifyRange(c(line, reg, line, reg + attr(reg, "match.length")), paste0("# .. ", n))
    }
  
  
  # ..  -----
  
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


