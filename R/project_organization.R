#' Create the basic project folder structure
#'
#' @param path the path of the project folder
#'
#' @return this function is called for its side-effect
#' @export
proj_create_folders <- function(path = "Work") {

  dirs <- c(
    file.path(path, c( "00-DataOriginal", "01-Data","02-Scripts/Resources", "03-Models", "04-Output", "05-Report", "06-Presentations"))
    )

  for (d in dirs)
    if (!dir.exists(file.path(d))) dir.create(file.path(d), recursive = TRUE)

  }


