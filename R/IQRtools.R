#' Rename a script, change filename occurences within script, rename output folder
#'
#' @param from,to as in file.rename 
#'
#' @export
rename_script <- function(from, to) {
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
    file.remove(file.path("../04-Output", from_stripped), recursive = TRUE)
  }
}