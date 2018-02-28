
#' Print ode vector in mathematica format
#'
#' @param myeqnlist an eqnlist
#'
#' @return
#' @export
#'
#' @examples
print_mathematica.eqnlist <- cf_print_mathematica.eqnlist <- function(myeqnlist) {myeqnlist %>%
    as.eqnvec() %>%
    print_mathematica.eqnvec}

#' Print ode vector in mathematica format
#'
#' @param myeqnvec an eqnvec
#'
#' @return
#' @export
#'
#' @examples
print_mathematica.eqnvec <- cf_print_mathematica.eqnvec <- function(myeqnvec) {myeqnvec %>%
{structure(as.character(.), names = names(.))} %>% .[order(names(.))] %>% paste(collapse = ", ")  %>%
    str_replace_all(c("_" = "")) %>%
    paste0("f={",.,"};")}


#' Print a character in mathematica format
#'
#' @param mycharacter
#'
#' @return
#' @export
#'
#' @examples
print_mathematica.character <- cf_print_mathematica.character <- function(mycharacter) {mycharacter %>%
    paste(collapse = ", ")  %>% str_replace_all(c("_" = "")) %>% paste0("x={",.,"};")}









#' Look at the argpath of a fit
#'
#' Requires the fit to be run with blather = T
#'
#' @param fit a fit returned from dMod::trust()
#'
#' @return a data.frame for plotting
#' @export
#'
#' @examples
prepare_argpath <- function(fit) {
  chisquare_values <- data.frame(iteration = 1:length(fit[["valpath"]]), parameter = "-2LogLikelihood", value = log(abs(fit[["valpath"]])), cluster = 0)

  fit %>%
    .$argpath %>%
    { myargpath <- .
    myclustering <- myargpath %>% t %>% kmeans(8) %>% .$cluster
    myargpath <- myargpath %>%
      as.data.frame() %>%
      set_names(names(fit$argument))%>%
      cbind(., iteration = 1:nrow(.)) %>%
      gather(.,"parameter", "value", 1:(length(.)-1)) %>%
      cbind(.,cluster = rep(myclustering, each = max(.$iteration)))
    } %>%
    mutate(parameter = parameter %>% as.factor) %>%
    mutate(cluster = as.factor(cluster)) %>%
    rbind(chisquare_values)

}

#' @rdname prepare_argpath
#'
#' @param argpath result from \link{prepare_argpath}
#'
#' @return a plot
#' @export
#'
#' @examples
plot_argpath <- function(argpath) {argpath %>% ggplot(aes(value, parameter, frame = iteration, color = cluster)) +
  geom_point()
}



#' Wait for runbg
#'
#' It's better to use \code{runbg(..., wait = T)}, if you want to wait for the results anyway
#'
#' But this was nice for practicing enquo()
#'
#' @param job
#' @param delta_t
#'
#' @return
#' @export
#'
#' @examples
wait_for_runbg <- function(job, delta_t=5) {

  while(!eval_tidy(quo("[["(!!enquo(job),"check")() )))
    Sys.sleep(delta_t)

}


# odemodel_arglist <- formals(odemodel)
# odemodel_arglist <- odemodel_arglist[!(names(odemodel_arglist) %in% c("..."))]
# odemodel_arglist[["f"]] <- f
# odemodel_arglist[["events"]] <- events
# odemodel_arglist[["modelname"]] <- paste0("odemodel_", digest::digest(odemodel_arglist[!(names(odemodel_arglist) %in% c("modelname"))]))
#
# file_exists <- file.exists(paste0(odemodel_arglist[["modelname"]], ".rda"))
# if (file_exists) {
#   myodemodel <- force(readRDS(paste0(odemodel_arglist[["modelname"]] , ".rda")))
# } else {
#   myodemodel <-  do.call(odemodel, odemodel_arglist)
#   saveRDS(myodemodel, paste0(odemodel_arglist[["modelname"]] , ".rda"))
# }

# Pimpl_arglist <- formals(Pimpl)
# Pimpl_arglist <- Pimpl_arglist[!(names(Pimpl_arglist) %in% c("..."))]
# Pimpl_arglist[["trafo"]] <- reactions %>% as.eqnvec()
# Pimpl_arglist[["parameters"]] <- c("S2", "S3", "S4", "TGFb", "Rec")
# Pimpl_arglist[["compile"]] <- T
# Pimpl_arglist[["modelname"]] <- paste0("pSS_", digest::digest(Pimpl_arglist[!(names(Pimpl_arglist) %in% c("modelname"))]))
#
# file_exists <- file.exists(paste0(Pimpl_arglist[["modelname"]], ".rda"))
# if (file_exists) {
#   pSS <- force(readRDS(paste0(Pimpl_arglist[["modelname"]] , ".rda")))
#   loadDLL(pSS)
# } else {
#   pSS <-  do.call(Pimpl, Pimpl_arglist)
#   saveRDS(pSS, paste0(Pimpl_arglist[["modelname"]] , ".rda"))
# }

