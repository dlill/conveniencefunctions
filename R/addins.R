#' Addin to insert tee-operator
#'
#'
#' @export
#'
#'
insert_tee_operator <- function ()
{
  rstudioapi::insertText(" %T>% ")
}

#' Addin to insert tee-operator
#'
#'
#' @export
#'
#'
insert_tee_print <- function ()
{
  rstudioapi::insertText("{.} %T>% print %>%")
}

#' Addin to insert tee-operator
#'
#'
#' @export
#'
#'
split_chunks <- function ()
{
  rstudioapi::insertText(
    "
```

```{r}
")
}





#' Addin to insert map(seq_along(x), function(i))
#'
#'
#' @export
#'
#'
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
#'
#' @export
#'
#'
describe_plotValue <- function(dMod.frame = NULL) {
  rstudioapi::insertText(paste0(paste0("In hypothesis ", dMod.frame$hypothesis, ": There are good bad steps\n")))
}

#' Addin
#'
#' @param dMod.frame
#'
#'
#' @export
#' @rdname describe_plotValue
#'
describe_plotCombined <- function(dMod.frame = NULL) {
  rstudioapi::insertText(paste0(paste0("In hypothesis ", dMod.frame$hypothesis, ": The predictions dont fit well/best.\n")))
}

#' @export
#' @rdname describe_plotValue
describe_plotPars <- function(dMod.frame = NULL) {
  rstudioapi::insertText(paste0(paste0("In hypothesis ", dMod.frame$hypothesis, ": The pars are spread out over a wide range.\n")))
}



#' @export
#' @rdname describe_plotValue
describe_plotPars <- function(dMod.frame = NULL) {
  rstudioapi::insertText(paste0(paste0("In hypothesis ", dMod.frame$hypothesis, ": The pars are spread out over a wide range.\n")))
}

#' @export
#' @rdname describe_plotValue
describe_plotProfile <- function(dMod.frame = NULL) {
  rstudioapi::insertText(paste0(paste0("In hypothesis ", dMod.frame$hypothesis, ": The pars  are structurally non-identifiable.\n  The pars  are practically non-identifiable")))
}



#' Insert a runbg job
#'
#' @param job_name character with "_job" at the end
#' @param job_type character, options: fit, profile, profile_steps
#' @param dMod.frame character "dMod.frame"
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' insert_runbg()
#'
#' insert_runbg(job_name = "myrunbg_job", job_type = "fit", dMod.frame = "myframe")
#' insert_runbg(job_name = "myrunbg_job", job_type = "profile", dMod.frame = "myframe")
#' myframe <- readRDS("~/Promotion/Projects/methacetin_fitting/Fit_model_41/2018_05_24_15_27_fit_krumbiegel_results.rds") %>% do.call(rbind,.)
#' insert_runbg(job_name = "myrunbg_job", job_type = "profile_steps", dMod.frame = "myframe", distribute = paste0("knecht", 1:5))
#' }
#'
insert_runbg <- function(job_name = "myrunbg_job", job_type = NULL, dMod.frame = NULL, tol = 1, nsteps = 3, distribute = NULL) {


  if(!is.null(job_type)&is.null(dMod.frame)) stop("supplying job_type works only when supplying a dMod.frame")

  # Defaults
  job_name <- job_name %>% str_replace("_job$", "") %>% paste0("_job")
  filename <- tpaste0(job_name)
  machine <- 'c(paste0("knecht", 1))'
  input <- 'ls(pos=.GlobalEnv)'
  if(!is.null(dMod.frame)) input <- dMod.frame


  # General Job ---
  rbg_header <- function() paste0('
', job_name, ' <- runbg({')

  rbg_body <- function() "\n\n"

  rbg_end <- function() paste0('\n}, machine = ', machine, ', input = "', input,'", filename = "', filename,'"
  # , recover = T
)')

  rbg_get_save_purge <- function() paste0('
# ',job_name,'$check()
# runbg(wait_for_runbg(',job_name,'), filename = "wait", input = "', job_name, '")
# ',str_replace(job_name, "_job", ""),' <- ',job_name,'$get()
# saveRDS(', str_replace(job_name, "_job", ""), ', file = "',str_replace(job_name, "_job", ""), '.rds")
# ', str_replace(job_name, "_job", ""), ' <- readRDS("',str_replace(job_name, "_job", ""), '.rds")
# ', job_name, '$purge()
')

  # Fit Job ----
  fit_body <- function() paste0('
    ncores <- detectFreeCores()
    assign("ncores", ncores, pos = .GlobalEnv)

   ', dMod.frame,' %>%
    ungroup %>%
    mutate(fits = map(seq_along(x), function(i) {
      assign("fit_obj", obj[[i]], pos = .GlobalEnv)
      assign("fit_pars", pars[[i]], pos = .GlobalEnv)
      assign("fit_fixed", NULL, pos = .GlobalEnv)
      # assign("fit_fixed", fixed[[i]], pos = .GlobalEnv)
      assign("fit_studyname", paste0("fits", hypothesis[[i]]), pos = .GlobalEnv)
      mstrust(objfun = fit_obj, center = fit_pars, fixed = fit_fixed, studyname = fit_studyname, sd = 3,
             blather = F, cores = ncores, fits = 10*ncores) })) %>%
    rowwise')


  # Profiles ---
  profile_body <- function() paste0('ncores <- detectFreeCores()
    assign("ncores", ncores, pos = .GlobalEnv)

    ', dMod.frame,' %>%
      ungroup %>%
      mutate(profiles = map(seq_along(x), function(i) {
        assign("fit_obj", obj[[i]], pos = .GlobalEnv)
        assign("fit_pars", parframes[[i]] %>% as.parvec, pos = .GlobalEnv)
        profile(obj = fit_obj, pars = fit_pars, whichPar = names(fit_pars), cores = ncores) })) %>%
      rowwise')

  # Profile steps ---
  profile_step_body <- function() paste0('   ncores <- detectFreeCores()
    assign("ncores", ncores, pos = .GlobalEnv)

   ', dMod.frame,' %>%
    ungroup %>%
    mutate(profiles = map(seq_along(x), function(i) {
      assign("fit_obj", obj[[i]], pos = .GlobalEnv)

      steps <- getSteps(parframes[[i]], tol = ', tol, ', nsteps = ', nsteps ,')
      lapply(steps, function(step) {
        assign("fit_pars", parframes[[i]] %>% as.parvec(step), pos = .GlobalEnv)
        profile(obj = fit_obj, pars = fit_pars, whichPar = names(fit_pars), cores = ncores)
      }) %>%
        list

       })) %>%
      rowwise')


  # Distribute ----
  if(!is.null(distribute)) {

    nhyp <- eval(parse(text = paste0("nrow(", dMod.frame, ")")))
    nknecht <- length(distribute)

    machine <- paste0(distribute, collapse = '","')

    mycut <- cut(1:nhyp, nknecht) %>% as.numeric()
    groups <- map(unique(mycut), function(i) {(1:nhyp)[mycut == i]})

    dummy_input <- input
    input <- "distributed_frame"
    dMod.frame <- input


    rbg_header <- function() paste0('
groups <- ', deparse(groups), '
', job_name, ' <- map(unique(groups), function(grp) {
    grp <<- grp
    distributed_frame <<- ',dummy_input,'[groups[[grp]],]
runbg({
')

    rbg_end <- function() paste0('}, machine = c("', machine,'")[grp], input = c("', input, '", "groups", "grp"),  filename = "', filename,'"
  # , recover = T
)
    })')

  rbg_get_save_purge <- function() paste0('
# map(',job_name,',. %>% .$check())
# ',str_replace(job_name, "_job", ""),' <- map(',job_name,',. %>% .$get())
# saveRDS(', str_replace(job_name, "_job", ""), ', file = "',str_replace(job_name, "_job", ""), '.rds")
# ', str_replace(job_name, "_job", ""), ' <- readRDS("',str_replace(job_name, "_job", ""), '.rds")
# map(', job_name, ',. %>% .$purge())
')
  }

if (is.null(job_type)) {
  job_text <- paste0(rbg_header(), rbg_body(), rbg_end(), rbg_get_save_purge(), collapse = "\n")
  rstudioapi::insertText(job_text)
} else {
  if(job_type == "fit") {
    job_text <- paste0(rbg_header(), fit_body(), rbg_end(), rbg_get_save_purge(), collapse = "\n")
    rstudioapi::insertText(job_text)
  } else if (job_type == "profile") {
    job_text <- paste0(rbg_header(), profile_body(), rbg_end(), rbg_get_save_purge(), collapse = "\n")
    rstudioapi::insertText(job_text)
  } else if (job_type == "profile_steps") {
    job_text <- paste0(rbg_header(), profile_step_body(), rbg_end(), rbg_get_save_purge(), collapse = "\n")
    rstudioapi::insertText(job_text)  }
}

}


