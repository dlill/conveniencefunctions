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




#' Describe plot
#'
#' Here are some templates to motivate you to describe the plots.
#'
#' @param dMod.frame
#'
#' @return
#' @export
#'
#' @examples
describe_plotValue <- function(dMod.frame = NULL) {
  rstudioapi::insertText(paste0(paste0("In hypothesis ", dMod.frame$hypothesis, ": There are good bad steps\n")))
}

#' Addin
#'
#' @param dMod.frame
#'
#' @return
#' @export
#' @rdname desribe_plotValue
#' @examples
describe_plotCombined <- function(dMod.frame = NULL) {
  rstudioapi::insertText(paste0(paste0("In hypothesis ", dMod.frame$hypothesis, ": The predictions dont fit well/best.\n")))
}

#' @export
#' @rdname desribe_plotValue
describe_plotPars <- function(dMod.frame = NULL) {
  rstudioapi::insertText(paste0(paste0("In hypothesis ", dMod.frame$hypothesis, ": The pars are spread out over a wide range.\n")))
}



#' @export
#' @rdname desribe_plotValue
describe_plotPars <- function(dMod.frame = NULL) {
  rstudioapi::insertText(paste0(paste0("In hypothesis ", dMod.frame$hypothesis, ": The pars are spread out over a wide range.\n")))
}

#' @export
#' @rdname desribe_plotValue
describe_plotProfile <- function(dMod.frame = NULL) {
  rstudioapi::insertText(paste0(paste0("In hypothesis ", dMod.frame$hypothesis, ": The pars  are structurally non-identifiable.\n  The pars  are practically non-identifiable")))
}
