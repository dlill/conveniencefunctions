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
#' cfoutput_MdTable(data.table(iris), split_by = "Species")
cfoutput_MdTable <- function(dt, split_by = NULL, filename = NULL, format = c("markdown", "pandoc"), 
                             caption = NULL, na.strings = "-", FLAGsummaryRow = TRUE, ...) {
  options(knitr.kable.NA = na.strings)
  
  # kt <- knitr::kable(dt,format = format[1], caption = caption)
  kt <- knitr::kable(dt,format = format[1], caption = caption, ...)
  
  seprow <- gsub(":","-",kt[2 + 2*(!is.null(caption))])
  widths <- nchar(strsplit(seprow, "|", fixed = TRUE)[[1]][-1])
  
  
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
  kt
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
