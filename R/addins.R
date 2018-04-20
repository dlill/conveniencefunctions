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
insert_runbg <- function(job_name = "runbg_job", job_type = NULL, dMod.frame = NULL) {

  filename <- tpaste0(job_name)

  if(!is.null(job_type)&is.null(dMod.frame)) stop("supplying job_type works only when supplying a dMod.frame")

  if (is.null(job_type)) {


    rstudioapi::insertText(paste0('
', job_name, ' <- runbg({

  },  machine = c(paste0("knecht", 1)), input = ls(pos=.GlobalEnv), filename = "', filename,'_runbg")
saveRDS(', job_name, ', file = "', filename, '.rds")
# ',job_name,' <- readRDS("', filename, '.rds")
# ',job_name,'$check()
# wait_for_runbg(',job_name,')
# ',job_name,'_results <- ',job_name,'$get()
# saveRDS(', job_name, '_results, file = "',filename, '_results.rds")
# ', job_name, '$purge()
# ', str_replace(job_name, "_job", ""), ' <- readRDS("',filename, '_results.rds")
'))


    } else {


    if(job_type == "fit") {


rstudioapi::insertText(paste0('
', job_name, ' <- runbg({
    ncores <- detectFreeCores()
    assign("ncores", ncores, pos = .GlobalEnv)

   ', dMod.frame,' %>%
    mutate(profiles = map(seq_along(x), function(i) {
      assign("fit_obj", obj[[i]], pos = .GlobalEnv)
      assign("fit_pars", pars[[i]], pos = .GlobalEnv)
      assign("fit_studyname", paste0("fits", hypothesis[[i]]), pos = .GlobalEnv)
      mstrust(objfun = fit_obj, center = fit_pars, studyname = fit_studyname, sd = 3,
             blather = F, cores = ncores, fits = 10*ncores) }))

  },  machine = c(paste0("knecht", 1)), input = "', dMod.frame,'", filename = "', filename,'_runbg")
saveRDS(', job_name, ', file = "', filename, '.rds")
# ',job_name,' <- readRDS("', filename, '.rds")
# ',job_name,'$check()
# wait_for_runbg(',job_name,')
# ',job_name,'_results <- ',job_name,'$get()
# saveRDS(', job_name, '_results, file = "',filename, '_results.rds")
# ', job_name, '$purge()
# ', str_replace(job_name, "_job", ""), ' <- readRDS("',filename, '_results.rds")
'))


      } else if (job_type == "profiles") {

rstudioapi::insertText(paste0('
', job_name, ' <- runbg({
    ncores <- detectFreeCores()
    assign("ncores", ncores, pos = .GlobalEnv)

   ', dMod.frame,' %>%
    mutate(profiles = map(seq_along(x), function(i) {
      assign("fit_obj", obj[[i]], pos = .GlobalEnv)
      assign("fit_pars", best_parvec[[i]], pos = .GlobalEnv)
      profile(obj = fit_obj, pars = fit_pars, whichPar = names(fit_pars), cores = ncores) }))

  },  machine = c(paste0("knecht", 1)), input = "', dMod.frame,'", filename = "', filename,'_runbg")
saveRDS(', job_name, ', file = "', filename, '.rds")
# ',job_name,' <- readRDS("', filename, '.rds")
# ',job_name,'$check()
# wait_for_runbg(',job_name,')
# ',job_name,'_results <- ',job_name,'$get()
# saveRDS(', job_name, '_results, file = "',filename, '_results.rds")
# ', job_name, '$purge()
# ', str_replace(job_name, "_job", ""), ' <- readRDS("',filename, '_results.rds")
'))


      }
  }

}


