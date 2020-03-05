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
assign_folders <- function(
  additional = NULL,
  scriptname = gsub(".R$", "", basename(rstudioapi::getSourceEditorContext()$path))
                           ) {
  folders <- c(.currentwd        = dirname(rstudioapi::getSourceEditorContext()$path),
               .outputFolder     = paste0("../04-Output/",scriptname),
               .modelFolder      = file.path(paste0("../04-Output/", scriptname), "01-Model"),
               .estimationFolder = file.path(paste0("../04-Output/", scriptname), "02-Estimation"),
               .plotFolder       = file.path(paste0("../04-Output/", scriptname), "03-Plot"),
               .tableFolder      = file.path(paste0("../04-Output/", scriptname), "04-Table"),
               .tempdir          = tempdir())
  folders <- c(folders, additional)
  
  for (x in seq_along(folders)){
    if (!dir.exists(folders[x])) dir.create(folders[x])
    assign(names(folders[x]), folders[x], .GlobalEnv)
  }
    assign(".tempfile", tempfile(), .GlobalEnv)
  
    cat("Assigned the following variables: ", paste0(c(names(folders), ".tempfile"), collapse = ", "), "\n")
}
