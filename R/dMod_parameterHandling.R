# The flow is like this
# * equations -> parameters_0_df 
# * parameters_0_df + estimate_info -> parameters_est_df + est.grid + fixed.grid
# * Fill with values manually
# repeat:
#   * parameters_est_df + est.grid + condition_info + estimate_info -> parameters_est_df + est.grid
#   * fixed.grid + condition_info -> fixed.grid
# * fixed.grid + est.grid + parameters_df + trafo_info -> trafo + parameters_est_df (with column est_value)

# [] Think of way how to fix one parameter at certain conditions while leaving other conditions free


#' Template to build a basic parameters_df
#'
#' @param odes,observables,errormodel symbolic definitions of functions
#'
#' @return data.frame with columns name0,name = name0, value = 1, unit = "a.u.", upper = 1e3 ,lower = 1e-5, 
#' estscale = "L", initpar = F, dynpar = F, obspar = F, errpar = F
#' @importFrom dplyr bind_rows
#' @export
cf_build_parameters_df <- function(odes, observables, errormodel) {
  pdf_initpars <- names(odes)
  pdf_initpars <- c(names(odes), paste0("init_", names(odes)))
  pdf_dynpars  <- setdiff(getSymbols(odes), pdf_initpars)
  pdf_obspars  <- setdiff(getSymbols(observables), c(names(observables), pdf_initpars, pdf_dynpars))
  pdf_errpars  <- setdiff(getSymbols(errormodel), c(pdf_initpars, pdf_dynpars))
  
  build_parameters_df_basic <- function(name0,name = name0, value = 1, unit = "a.u.", upper = 1e3 ,lower = 1e-5, 
                                        estscale = "L", FLAGinitpar = F, FLAGdynpar = F, FLAGobspar = F, FLAGerrpar = F){
    data.frame(name0 = name0, name = name, value = value, upper = upper, lower = lower, 
               estscale = estscale, FLAGinitpar = FLAGinitpar, FLAGdynpar = FLAGdynpar, FLAGobspar = FLAGobspar, FLAGerrpar = FLAGerrpar, 
               stringsAsFactors = FALSE)}
  
  dplyr::bind_rows(
    build_parameters_df_basic(pdf_initpars, FLAGinitpar = T),
    build_parameters_df_basic(pdf_dynpars, FLAGdynpar = T),
    build_parameters_df_basic(pdf_obspars, FLAGobspar = T),
    build_parameters_df_basic(pdf_errpars, FLAGerrpar = T)
  )
}




#' Make a column of an est.grid condition specific and augment est.vec
#' 
#' new parameters will be named parname_"condition_column"
#' 
#' @param est.grid,parameters_est_df as usual
#' @param parname character(1L) denoting a column in the est.grid
#' @param conditions character vector of conditions
#' @param FLAGdummifyOtherConds replace the other parameters by "dummy". Dummy will have value c(dummy = 1)
#' 
#' @export 
cf_make_condition_specific <- function(est.grid, parameters_est_df, parname, conditions, condition_column = "ID", FLAGdummifyOtherConds = FALSE){
  condition_indices <- est.grid[["condition"]] %in% conditions
  
  parnames <- est.grid[condition_indices, parname, drop = TRUE]
  est.grid[condition_indices, parname] <- paste0(est.grid[condition_indices, parname], "_", est.grid[condition_indices, condition_column])
  parnames_new <- est.grid[condition_indices, parname, drop = TRUE]
  
  .x <- unique(parnames)[1]
  for (.x in unique(parnames)){
    parnames_new_.x   <- parnames_new[parnames == .x]
    est.vec_df_new    <- filter(parameters_est_df, name == .x) %>% mutate(name = list(parnames_new_.x)) %>% unnest(name)
    parameters_est_df <- bind_rows(parameters_est_df, est.vec_df_new)
  }
  
  if (FLAGdummifyOtherConds) {
    condition_indices <- !(est.grid[["condition"]] %in% conditions)
    parnames <- unique(est.grid[[parname]])
    est.grid[condition_indices, parname] <- "dummy"
    parnames <- unique(est.grid[[parname]])
  }
  
  return(list(est.grid = est.grid, est.vec_df = parameters_est_df))
}


#' Make parameters_df parameters condition specific
#'
#' will be done like this: parname_condition
#'
#' @param parameters_df a typical parameters_df
#' @param conditions character vector.
#' @param parnames names of parameters to apply condition specificity to
#'
#' @return new paraemters_df with new rows
#' @export
cf_parameters_df_condition_specific <- function(parameters_df, conditions = c("C1", "C2"), parnames = parameters_df$name) {
  # possible options
  # * keep or drop original parameters?
  # * ...
  parnames_new <- outer(parnames, conditions, paste, sep = "_")
  x <- unique(parnames)[1]
  for (x in unique(parnames)){
    parnames_new_x   <- parnames_new[parnames == x]
    parameters_df_new    <- filter(parameters_df, name == x) %>% 
      mutate(name = list(parnames_new_x)) %>% unnest(name)
    parameters_df <- bind_rows(parameters_df, parameters_df_new)
  }
  parameters_df
}


#' Merge values into a parameters_df
#'
#' @param pars_df_into parameters_df
#' @param from Either named vector or parameters_df
#'
#' @return pars_df_into with updated values
#' @export
cf_merge_values_parameters_df <- function(pars_df_into, from) {
  # possible options
  # * match name0 instead/as well
  # * merge scales/upper/lower/FLAGs?
  if (!is.data.frame(from))
    from <- data.frame(name = names(from), value = from, stringsAsFactors = F)
  p <- merge(pars_df_into, select(from, name, VALOLD = value), all.x = TRUE, all.y = FALSE)
  p <- mutate(p, value = case_when(!is.na(VALOLD) ~ VALOLD, TRUE ~ value))
  p <- select(p, -VALOLD)
  p
}

#' @export
unclass_parvec <- function(x) {setNames(unclass(x)[1:length(x)], names(x))}





