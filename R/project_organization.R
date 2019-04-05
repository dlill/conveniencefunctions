#' Create the basic project folder structure
#'
#' @param path the path of the project folder
#'
#' @return this function is called for its side-effect
#' @export
proj_create_folders <- function(path) {

    dirs <- c("Scripts", "Data", "Outputs", "Documentation")

  for (d in dirs) {
    if (!dir.exists(file.path(d)))
      dir.create(file.path(d))
  }

}


