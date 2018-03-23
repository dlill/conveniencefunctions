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


#' Addin to insert map(seq_along(x), function(i))
#'
#' @return
#' @export
#'
#' @examples
insert_seq_along_x_function_i <- function ()
{
  rstudioapi::insertText("seq_along(x), function(i) ")
}
