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

#' Create output folders and assign names to variables
#'
#' @param scriptname character
#'
#' @export
assign_folders <- function(scriptname) {
  folders <- c(.currentwd        = rstudioapi::getSourceEditorContext()$path,
               .outputFolder     = paste0("../04-Output/",scriptname),
               .modelFolder      = file.path(paste0("../04-Output/", scriptname), "01-Model"),
               .estimationFolder = file.path(paste0("../04-Output/", scriptname), "02-Estimation"),
               .plotFolder       = file.path(paste0("../04-Output/", scriptname), "03-Plot"),
               .tableFolder      = file.path(paste0("../04-Output/", scriptname), "04-Table"),
               .tempdir          = tempdir())
  for (x in folders){
    if (!dir.exists(x)) dir.create(x)
    assign(names(x), x, .GlobalEnv)
  }
}
