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


#' @export
#' @rdname insert runbg()
insert_runbg <- function(job_name = "runbg_job") {
  filename <- tpaste0(job_name, ".rds")
  rstudioapi::insertText(paste0(paste0('
', job_name, ' <- runbg({

  },  machine = c(paste0("knecht", 1)), input = ls(pos=.GlobalEnv) )
saveRDS(', job_name, ', filename = "', filename, '")
#',job_name,' <- readRDS("', filename, '")
#',job_name,'$check()
#',job_name,'_results <- ',job_name,'$get()
#saveRDS(', job_name, '_results, filename = tpaste0("', job_name, '_results.rds"))
#', job_name, '$purge()
#', str_replace(job_name, "_job", ""), ' <- readRDS(tpaste0("', job_name, '_results.rds")
')))
}



