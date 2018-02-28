#' Addin to insert tee-operator
#'
#' @return
#' @export
#'
#' @examples
insert_tee_operator <- function ()
{
  rstudioapi::insertText(" %T>% ")
}

#' Addin to insert tee-operator
#'
#' @return
#' @export
#'
#' @examples
insert_tee_print <- function ()
{
  rstudioapi::insertText(" %T>%
  print %>%
  {.}")
}

