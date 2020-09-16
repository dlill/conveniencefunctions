#' Unnest list column
#'
#' @param dt data.table with a list.column which is a list of data.tables
#' @param colname name of column with list(data.tables...)
#'
#' @return data.table with the list column unnested
#' @export
#'
#' @examples
#' dt <- data.table(a = 1:2, b = list(data.table(listdt1 = 1:3, listdt2 = 2:4), data.table(listdt1 = 4:6, listdt2 = 8:10) ))
#' colname <- "b"
#' dt1 <- unnest_listcol_dt(dt, colname)
#' dt1
unnest_listcol_dt <- function(dt, colname) {
  dt[,`:=`(.rowid. = as.character(1:nrow(dt)))]
  dtout <- rbindlist(setNames(dt[[colname]], dt$.rowid.), idcol = ".rowid.")
  dtout <- dtout[dt[,!c(..colname)], on = c(".rowid.")]
  dtout[,`:=`(.rowid. = NULL)]
  dtout
}


