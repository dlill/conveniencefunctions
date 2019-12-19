#' Title
#'
#' @param template 
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom officer read_pptx
cf_slides <- function(template = 1, templatelist = system.file("templates/Uni_Praesentation_E1_RGB_16_9.potx", package = "conveniencefunctions")) {
  read_pptx(templatelist[template])
}
# [] Incorporate uni slides



#' Title
#'
#' @param .slides 
#' @param title 
#' @param content1 
#' @param content2 
#' @param footer 
#' @param slide_num 
#'
#' @return
#' @export
#' 
#' @importFrom officer add_slide ph_with ph_location_type
#'
#' @examples
cf_slide_twocontent <- function(.slides, title, content1, content2, footer = "", slide_num = "") {
  .slides <- officer::add_slide(.slides, "Two Content")
  .slides <- officer::ph_with(.slides, value = title, location = officer::ph_location_type(type = "title"))
  .slides <- officer::ph_with(.slides, value = footer, location = officer::ph_location_type(type = "ftr"))
  .slides <- officer::ph_with(.slides, value = format(Sys.Date()), location = officer::ph_location_type(type = "dt"))
  .slides <- officer::ph_with(.slides, value = slide_num, location = officer::ph_location_type(type = "sldNum"))
  .slides <- officer::ph_with(.slides, value = content, location = officer::ph_location_type(type = "body", FALSE)) 
  .slides <- officer::ph_with(.slides, value = content2, location = officer::ph_location_type(type = "body",TRUE)) 
  .slides
}

#' Title
#'
#' @param .slides 
#' @param title 
#' @param content 
#' @param footer 
#' @param slide_num 
#'
#' @return
#' @export
#'
#' @examples
cf_slide_titlecontent <- function(.slides, title, content, footer = "", slide_num = "") {
  .slides <- officer::add_slide(.slides)
  .slides <- officer::ph_with(.slides, value = title, location = officer::ph_location_type(type = "title"))
  .slides <- officer::ph_with(.slides, value = footer, location = officer::ph_location_type(type = "ftr"))
  .slides <- officer::ph_with(.slides, value = format(Sys.Date()), location = officer::ph_location_type(type = "dt"))
  .slides <- officer::ph_with(.slides, value = slide_num, location = officer::ph_location_type(type = "sldNum"))
  .slides <- officer::ph_with(.slides, value = content, location = officer::ph_location_type(type = "body")) 
  .slides
}

#' Title
#'
#' @param .slides 
#' @param title 
#' @param footer 
#' @param slide_num 
#'
#' @return
#' @export
#'
#' @examples
cf_slide_section <- function(.slides, title, footer = "", slide_num = "") {
  .slides <- officer::add_slide(.slides, "Section Header")
  .slides <- officer::ph_with(.slides, value = title, location = officer::ph_location_type(type = "title"))
  .slides
}




