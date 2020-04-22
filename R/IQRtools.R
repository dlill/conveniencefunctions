#' Rename a script, change filename occurences within script, rename output folder
#'
#' @param from,to as in file.rename 
#'
#' @export
cf_rename_script <- function(from, to) {
  cf_copy_script(from, to, TRUE)
}



#' Rename a script, change filename occurences within script, rename output folder
#'
#' @param from,to as in file.rename 
#'
#' @export
cf_copy_script <- function(from, to, FLAGremoveOld = FALSE) {
  
  from_stripped <- stringr::str_replace_all(from, c("\\.R$" = "", "^SCRIPT_" = ""))
  to_stripped   <- stringr::str_replace_all(to,   c("\\.R$" = "", "^SCRIPT_" = ""))
  
  if (FLAGremoveOld) {
    for (y in list.files()) {
      x <- readLines(y)
      if (sum(stringr::str_count(ln, from_stripped)) > 0)
        message(x, ": ", sum(stringr::str_count(ln, from_stripped)), "-------\n")
      x <- stringr::str_replace_all(x, from_stripped, to_stripped)
      writeLines(x,y)
    }
    file.copy(from, to)
    if(FLAGremoveOld) unlink(from)
    
    try(system(paste0("mv ../04-Output/",from_stripped, " ../04-Output/",to_stripped)))
    
  } else {
  ln <- readLines(from)
  message("Number of replaced filename references: ", sum(stringr::str_count(ln, from_stripped)), "-------\n")
  ln <- stringr::str_replace_all(ln, from_stripped, to_stripped)
  writeLines(ln, to)
  }
}



