#' Title
#'
#' @param dirnm 
#'
#' @return
#' @export
#'
#' @examples
cf_dir.create <- function(dirnm) {
  if (!dir.exists(dirnm)) dir.create(dirnm, recursive = TRUE)
  dirnm
}


#' Output table as markdown
#'
#' @param dt data.table
#' @param split_by Charactor of colnames to split the table by inserting separator lines
#' @param filename 
#'
#' @return object of class knitr::kable
#' 
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
#'
#' @examples
#' dt <- data.table(iris)[1:5]
#' cfoutput_MdTable(dt)
#' cfoutput_MdTable(data.table(iris), split_by = "Species")
cfoutput_MdTable <- function(dt, split_by = NULL, filename = NULL, format = c("markdown", "pandoc"), 
                             caption = NULL, na.strings = "-", FLAGsummaryRow = TRUE, 
                             NFLAGtribble = 0,
                             ...) {
  options(knitr.kable.NA = na.strings)
  
  # kt <- knitr::kable(dt,format = format[1], caption = caption)
  kt <- knitr::kable(dt,format = format[1], caption = caption, ...)
  
  seprow <- gsub(":","-",kt[2 + 2*(!is.null(caption))])
  widths <- nchar(strsplit(seprow, "|", fixed = TRUE)[[1]][-1])
  
  if (NFLAGtribble) {
    types <- vapply(dt, class, "double")
    for (fact in names(types)[types=="factor"]) dt[[fact]] <- as.character(dt[[fact]])
    for (fact in names(types)[types%in%c("character","factor")]) dt[[fact]] <- paste0('"', dt[[fact]],'"')
    kt <- knitr::kable(dt,format = "markdown")
    kt <- kt[-(1:2)]
    row0 <- paste0("data.table(tibble::tribble(")
    row1 <- paste0(paste0("~", names(dt), ","), collapse = " ")
    kt <- substr(kt, 2, nchar(kt))
    kt <- gsub("\\|", ",", kt)
    rowN <- kt[length(kt)]
    rowN <- substr(rowN, 1, nchar(rowN)-1)
    rowN <- paste0(rowN, "))")
    kt <- kt[-length(kt)]
    kt <- c(row0, row1, kt, rowN, "")
    if (NFLAGtribble == 1) cat(kt, sep = "\n")
    if (NFLAGtribble == 2) {
      e <- rstudioapi::getSourceEditorContext()
      rstudioapi::documentSave(e$id)
      rstudioapi::insertText(e$selection[[1]]$range$start, paste0(kt, collapse = "\n"))
    }
    return(invisible(kt))
  }
  
  if (!is.null(split_by)){
    dt <- copy(as.data.table(dt))
    n <- dt[,list(nlines = .N), by = split_by]
    n[,`:=`(rowid = cumsum(nlines))]
    n[,`:=`(rowid = rowid + 2 + 2*(!is.null(caption)))]
    atr <- attributes(kt)
    for (i in rev(n$rowid)[-1]) {
      kt <- c(kt[1:i], seprow, kt[(i+1):length(kt)])
    }
    attributes(kt) <- atr
  }
  
  if (FLAGsummaryRow) {
    summaryrow <- vapply(dt, function(x) paste0("N=", length(unique(x))), "N=nunique")
    summaryrow <- vapply(seq_along(summaryrow), function(i) sprintf(paste0("%",widths[i],"s"), summaryrow[i]), "bla")
    summaryrow <- paste0("|", paste0(summaryrow, collapse = "|"), "|")
    kt <- c(kt, seprow, summaryrow)
  }
  
  if(!is.null(filename))
    writeLines(kt, filename)
  
  cat(kt, sep = "\n")
  invisible(kt)
}



#' Title
#'
#' @param x 
#' @param filename 
#'
#' @return
#' @export
#'
#' @examples
cfwriteLines <- function(x, filename) {
  cf_dir.create(dirname(filename))
  writeLines(x, filename)
}

#' Title
#'
#' @param object 
#' @param filename 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
cfoutput_RDS <- function(object, filename, ...) {
  cf_dir.create(dirname(filename))
  saveRDS(object, file = filename, ...)
}
