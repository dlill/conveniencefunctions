#' Rename a script, change filename occurences within script, rename output folder
#'
#' @param from,to as in file.rename 
#'
#' @export
cf_rename_script <- function(from, to) {
  # 1 rename file
  file.rename(from, to)
  # 2 rename filename within script
  ln <- readLines(to)
  from_stripped <- str_replace_all(from, "\\.R$", "")
  to_stripped <- str_replace_all(to, "\\.R$", "")
  ln <- str_replace_all(ln, from_stripped, to_stripped)
  writeLines(ln, to)
  # 3 rename output folder
  if (dir.exists(file.path("../04-Output", from_stripped))){
    dir.create(file.path("../04-Output", to_stripped))
    file.copy(file.path("../04-Output", from_stripped), file.path("../04-Output", to_stripped), recursive = TRUE)
    unlink(file.path("../04-Output", from_stripped), recursive = TRUE)
  }
}



#' Rename a script, change filename occurences within script, rename output folder
#'
#' @param from,to as in file.rename 
#'
#' @export
cf_copy_script <- function(from, to, FLAGremoveOld = FALSE) {
  ln <- readLines(from)
  from_stripped <- stringr::str_replace_all(from, c("\\.R$" = "", "^SCRIPT_" = ""))
  to_stripped   <- stringr::str_replace_all(to,   c("\\.R$" = "", "^SCRIPT_" = ""))
  message("Number of replaced filename references: ", sum(stringr::str_count(ln, from_stripped)), "-------\n")
  ln <- stringr::str_replace_all(ln, from_stripped, to_stripped)
  writeLines(ln, to)
  if(FLAGremoveOld) unlink(from)
  NULL
}



