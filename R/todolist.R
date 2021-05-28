#' Todolist
#'
#' @param path path to search through scripts
#' @param FLAGrecursive search subfolders?
#' @param FLAGremoveDone remove done bullet points
#'
#' @return markdown table printed to console
#' @export
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @family 
#' @importFrom data.table data.table rbindlist copy
#' @importFrom stringr str_extract str_remove str_detect fixed
todolist <- function(path = ".", FLAGrecursive = FALSE, FLAGremoveDone = TRUE) {
  
  files <- list.files(path, "\\.(R|md|txt)$", recursive = FLAGrecursive, full.names = TRUE)
  d <- lapply(setNames(files, basename(files)), function(f) {
    ft <- grep("^(# )?\\[[x ]*\\]", readLines(f), value = TRUE)
    data.table::data.table(gout = ft)
  })
  
  d <- data.table::rbindlist(d, idcol = "script")
  
  d[,`:=`(scriptn = data.table::copy(script))]
  d[,`:=`(scriptn = stringr::str_extract(scriptn, "S\\d+(-\\d+)?"))]
  d[,`:=`(scriptn = stringr::str_remove(scriptn, "S"))]
  d[is.na(scriptn),`:=`(scriptn = script)]
    
  d[,`:=`(done = stringr::str_detect(gout, "\\[x\\]"))]
  d[,`:=`(check = ifelse(done, "[x]", "[ ]"))]
  
  d[,`:=`(task = data.table::copy(gout))]
  d[,`:=`(task = stringr::str_remove(task, "^# ?"))]
  d[,`:=`(task = stringr::str_remove(task, stringr::fixed(check)))]
  
  d <- d[order(done, scriptn)]
  if (FLAGremoveDone) d <- d[done != TRUE]
  d <- d[,list(check, scriptn, task)]
  
  cfoutput_MdTable(d, "check")
}


