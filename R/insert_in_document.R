# insert runbg----

#' Insert a runbg job
#'
#' @param job_name character with "_job" at the end
#' @param job_type character, options: fit, profile, profile_steps
#' @param dMod.frame character "dMod.frame"
#'
#' @param tol 1 used for profile_steps
#' @param nsteps 3
#' @param distribute vec of names of ssh machines c("knecht1", "knecht2")
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

  # Defaults ----
  job_name <- job_name %>% str_replace("_job$", "") %>% paste0("_job")
  filename <- tpaste0(job_name)
  machine <- 'c(paste0("knecht", 1))'
  input <- 'ls(pos=.GlobalEnv)'
  if(!is.null(dMod.frame)) input <- dMod.frame


  # Header----
  rbg_header <- function() paste0('
# ---------------------------------------------------------- #
# Runbg: ',str_replace(job_name, "_job$", ""), ' ----
# ---------------------------------------------------------- #
setwd(here("Outputs", outdir))
', job_name, ' <- runbg({')

  rbg_body <- function() "\n\n"

  rbg_end <- function() paste0('

     out
\n}, machine = ', machine, ', input = "', input,'", filename = "', filename,'"
  # , recover = T
)')

  # GetSavePurge ----
  rbg_get_save_purge <- function() paste0('
# ----------------------------------------------- #
# .. Description ----
# ----------------------------------------------- #
# ',job_name,' calculates ', job_type, 's for model ...
# The primary goal is to ...
#
#
# ----------------------------------------------- #
# .. get/save/purge ----
# ----------------------------------------------- #
setwd(here("Outputs", outdir))
# ',job_name,'$check()
# ',str_replace(job_name, "_job", ""),' <- ',job_name,'$get()
# ',str_replace(job_name, "_job", ""),' %>% str1
# ',str_replace(job_name, "_job", ""),' %>% map(list("fits", 1)) %>% unlist(F) %>% map("value") %>% reduce(c)
# ',str_replace(job_name, "_job", ""),' %>% map("fits") %>% unlist(F) %>% reduce(c) %>% saveRDS("fitlist_',str_replace(job_name, "_job", ""),'.rds")
# ',str_replace(job_name, "_job", ""),' <- ', str_replace(job_name, "_job", ""), ' %>% uniteFits %>% appendParframes %>% mutate(parframes = list(add_stepcolumn(parframes)))
# saveRDS(', str_replace(job_name, "_job", ""), ', file = "',str_replace(job_name, "_job", ""), '.rds")
# ', dMod.frame, ' <- readDMod.frame("',str_replace(job_name, "_job", ""), '.rds")
# ', job_name, '$purge()
')

  # Body ----
  # .. Fitjob ----
  fit_body <- function() paste0('
    ncores <- detectFreeCores()
    assign("ncores", ncores, pos = .GlobalEnv)

   out <- ', dMod.frame,' %>%
    ungroup %>%
    mutate(fits = map(seq_along(x), function(i) {
      checkout_hypothesis(', dMod.frame,', i, "fit_")

      mstrust(objfun = fit_obj,
              center = fit_center,
              fixed = fit_fixed,
              studyname = "fits",
              blather = F,
              cores = ncores,
              conditions = fit_conditions,
              parupper = fit_parupper,
              parlower = fit_parlower,
              iterlim = 300
      ) })) %>%
    rowwise')


  # .. Profiles ----
  profile_body <- function() paste0('ncores <- detectFreeCores()
    assign("ncores", ncores, pos = .GlobalEnv)

    out <- ', dMod.frame,' %>%
      ungroup %>%
      mutate(profiles = map(seq_along(x), function(i) {
        assign("fit_obj", obj[[i]], pos = .GlobalEnv)
        assign("fit_pars", parframes[[i]] %>% as.parvec, pos = .GlobalEnv)
        profile(obj = fit_obj, pars = fit_pars, whichPar = names(fit_pars), cores = ncores) })) %>%
      rowwise')

  # .. Profile steps ----
  profile_step_body <- function() paste0('   ncores <- detectFreeCores()
    assign("ncores", ncores, pos = .GlobalEnv)

   out <- ', dMod.frame,' %>%
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


  # .. Distribute ----
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
# ----------------------------------------------- #
# .. get/save/purge ----
# ----------------------------------------------- #
# map(',job_name,',. %>% .$check())
# ',str_replace(job_name, "_job", ""),' <- map(',job_name,',. %>% .$get())
# save(.runbgOutput, file = "runbgOutput.rda")
# saveRDS(', str_replace(job_name, "_job", ""), ', file = "',str_replace(job_name, "_job", ""), '.rds")
# ', str_replace(job_name, "_job", ""), ' <- readDMod.frame("',str_replace(job_name, "_job", ""), '.rds")
# map(', job_name, ',. %>% .$purge())
')
  }

  # Print ----

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



#' Print a header to your console
#' 
#' @param insert_in_script boolean
#'
#' @export
#'
insert_header <- function(insert_in_script = T){
  header <- paste0(
    "# ---------------------------------------------------------- #", "\n",
    "# Purpose ----",                                                   "\n",
    "# ---------------------------------------------------------- #", "\n",
    "# ", "\n",
    "# ", "\n",
    "# ", "\n",
    "\n",
    "# ---------------------------------------------------------- #", "\n",
    "# Dependencies (Scripts) ----",                                 "\n",
    "# ---------------------------------------------------------- #", "\n",
    "# ", "\n",
    "# ", "\n",
    "# ", "\n",
    "\n",
    "# ---------------------------------------------------------- #", "\n",
    "# Header ----",                                                   "\n",
    "# ---------------------------------------------------------- #", "\n",
    "rm(list = ls())",                                             "\n",
    "library(conveniencefunctions)",                               "\n",
    "setwd(here())", "\n",
    "\n",
    '#source(here("Scripts/S00 Auxiliaries.R"))', "\n",
    "fast <- TRUE", "\n",
    "\n",
    'outdir <- "', str_extract(basename(rstudioapi::getActiveDocumentContext()$path), "S[0-9]*"), '_outputs"', "\n",
    "if (!dir.exists(here('Outputs', outdir)))", "\n",
    "  dir.create(here('Outputs', outdir))", "\n",
    "\n",
    '.base_name <- "plots_', str_extract(basename(rstudioapi::getActiveDocumentContext()$path), "S[0-9]*"), '"', "\n",
    "next_file(purge = TRUE)", "\n"
  )
  return(header)
}


#' @export
#' @rdname insert_header
insert_exit <- function(insert_in_script = T){
  exit <- paste0(
    "# ---------------------------------------------------------- #", "\n",
    "# Exit ----",                                                   "\n",
    "# ---------------------------------------------------------- #", "\n",
    'setwd(here("Outputs", outdir))', "\n",
    '# save.image("workspace_', str_extract(basename(rstudioapi::getActiveDocumentContext()$path), "S[0-9]*"), '.rda")', "\n",
    "unlink_dMod()" , "\n",
    "while(dev.cur() > 1){", "\n",
    "  dev.off()", "\n",
    "}", "\n",
    "\n",
    "\n",
    "\n",
    "\n",
    "\n",
    "\n"
    
  )
  return(exit)
}



#' inserts function assignment of an r-function into the sript
#'
#' @param f a function as name
#' @return called for its side effect
#' 
#' @export
#' @importFrom rstudioapi insertText
insert_function <- function(f) {
  fname <- as.character(substitute(f))
  fbody <- capture.output(print(f))
  fbody <- fbody[- (length(fbody) - 0:1)]
  fbody[1] <- paste0(fbody[1], "{")
  fbody[2:length(fbody)] <- paste0("  ", fbody[2:length(fbody)])
  fbody <- c(fbody, "}")
  fbody <- paste0(fbody, collapse = "\n")
  paste0("\n\n", fname, " <- ", fbody, "\n\n") %>% rstudioapi::insertText()
}
