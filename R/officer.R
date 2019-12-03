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
#' @examples
cf_slide_twocontent <- function(.slides, title, content1, content2, footer = "", slide_num = "") {
  .slides <- add_slide(.slides, "Two Content")
  .slides <- ph_with(.slides, value = title, location = ph_location_type(type = "title"))
  .slides <- ph_with(.slides, value = footer, location = ph_location_type(type = "ftr"))
  .slides <- ph_with(.slides, value = format(Sys.Date()), location = ph_location_type(type = "dt"))
  .slides <- ph_with(.slides, value = slide_num, location = ph_location_type(type = "sldNum"))
  .slides <- ph_with(.slides, value = content, location = ph_location_type(type = "body", FALSE)) 
  .slides <- ph_with(.slides, value = content2, location = ph_location_type(type = "body",TRUE)) 
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
  .slides <- add_slide(.slides)
  .slides <- ph_with(.slides, value = title, location = ph_location_type(type = "title"))
  .slides <- ph_with(.slides, value = footer, location = ph_location_type(type = "ftr"))
  .slides <- ph_with(.slides, value = format(Sys.Date()), location = ph_location_type(type = "dt"))
  .slides <- ph_with(.slides, value = slide_num, location = ph_location_type(type = "sldNum"))
  .slides <- ph_with(.slides, value = content, location = ph_location_type(type = "body")) 
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
  .slides <- add_slide(.slides, "Section Header")
  .slides <- ph_with(.slides, value = title, location = ph_location_type(type = "title"))
  .slides
}
