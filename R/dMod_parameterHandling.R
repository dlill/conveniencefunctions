# The flow is like this
# * equations -> parameters_0_df 
# * parameters_0_df + estimate_info -> parameters_est_df + est.grid + fixed.grid
# * Fill with values manually
# repeat:
#   * parameters_est_df + est.grid + condition_info + estimate_info -> parameters_est_df + est.grid
#   * fixed.grid + condition_info -> fixed.grid
# * fixed.grid + est.grid + parameters_df + trafo_info -> trafo + parameters_est_df (with column est_value)

# [] Think of way how to fix one parameter at certain conditions while leaving other conditions free


#' Build basic fixed.grid, est.grid, est.vec_df
#'
#' @param parameters_df 
#' @param parameters_estimate 
#' @param condition.grid 
#' @param condition.grid_parcolumns names of cols which have parameter values in them
#'
#' @return
#' @export
#'
#' @examples
cf_build_pargrids <- function(parameters_df, parameters_estimate, condition.grid, condition.grid_parcolumns = NULL) {
  fixed.grid <- parameters_df %>%
    filter(!name0 %in% c(parameters_estimate, intersect(name0, names(condition.grid)))) %>%
    reshape2::dcast(. ~ name, value.var = "value") %>% .[-1] %>% 
    cbind(condition.grid[c("ID", "condition", condition.grid_parcolumns)], .)
  
  est.grid <- parameters_df %>%
    filter(name0 %in% parameters_estimate) %>%
    reshape2::dcast(. ~ name0, value.var = "name") %>%
    select(-1) %>%
    cbind(condition.grid[c("ID", "condition")], .)
  est.vec_df <- parameters_df %>% filter(name0 %in% parameters_estimate)
  list(fixed.grid = fixed.grid, est.grid = est.grid, est.vec_df = est.vec_df)
}


#' Title
#'
#' @param condition_specific tibble with columns 
#'   char "parname", 
#'   list of chars "conditions" 
#'   char "condition_column" => refers to colname in condition.grid
#'   lgl FLAGdummifyOtherConds
#' @param est.grid 
#' @param est.vec_df 
#' @param condition.grid 
#'
#' @return
#' @export
#'
#' @examples
cf_condition_specific_wrapper <- function(condition_specific, est.grid, est.vec_df, condition.grid) {
  # Make replacements for condition specificity
  for  (i in 1:nrow(condition_specific)) {
    wup <- cf_make_condition_specific(est.grid, est.vec_df, 
                                      parname = condition_specific$parname[[i]], 
                                      conditions = condition_specific$conditions[[i]], 
                                      condition_column = condition_specific$condition_column[[i]],
                                      FLAGdummifyOtherConds = condition_specific$FLAGdummifyOtherConds[[i]],
                                      condition.grid = condition.grid
    )
    est.grid <- wup$est.grid
    est.vec_df  <- wup$est.vec_df
  }
  # Subset est.vec_df to pars existing in the est.grid
  est.vec_df <- est.vec_df %>% 
    filter(name %in% unique(do.call(c, select(est.grid, -ID, -condition))))
  list(est.grid = est.grid, est.vec_df = est.vec_df)
}


#' Template to build a basic parameters_df
#'
#' @param odes,observables,errormodel symbolic definitions of functions
#' @param FLAGguessEstScale Set estscale to "N" for parameters with certain regexes, e.g. "^offset_"
#' 
#' 
#' @return data.frame with columns name0,name = name0, value = 1, unit = "a.u.", upper = 1e3 ,lower = 1e-5, 
#' estscale = "L", initpar = F, dynpar = F, obspar = F, errpar = F
#' @importFrom dplyr bind_rows
#' @export
cf_build_parameters_df <- function(odes, observables, errormodel, FLAGguessEstScale = TRUE) {
  
  # [] implement getting parameters from events
  
  pdf_initpars <- names(odes)
  pdf_dynpars  <- setdiff(getSymbols(odes), pdf_initpars)
  pdf_obspars  <- setdiff(getSymbols(observables), c(names(observables), pdf_initpars, pdf_dynpars))
  pdf_errpars  <- setdiff(getSymbols(errormodel), c(pdf_initpars, pdf_dynpars, pdf_obspars, names(errormodel)))
  
  build_parameters_df_basic <- function(name0,name = name0, value = 1, unit = "a.u.", upper = 1e3 ,lower = 1e-5, 
                                        estscale = "L", FLAGinitpar = F, FLAGdynpar = F, FLAGobspar = F, FLAGerrpar = F,
                                        FLAGindividualized = FALSE){
    if (!length(name0))
      return(NULL)
    data.frame(name0 = name0, name = name, value = value, upper = upper, lower = lower, 
               estscale = estscale, FLAGinitpar = FLAGinitpar, FLAGdynpar = FLAGdynpar, 
               FLAGobspar = FLAGobspar, FLAGerrpar = FLAGerrpar, 
               FLAGindividualized = FLAGindividualized,
               stringsAsFactors = FALSE)}
  
  parameters_df <- dplyr::bind_rows(
    build_parameters_df_basic(pdf_initpars, FLAGinitpar = T, value = 0),
    build_parameters_df_basic(pdf_dynpars, FLAGdynpar = T),
    build_parameters_df_basic(pdf_obspars, FLAGobspar = T),
    build_parameters_df_basic(pdf_errpars, FLAGerrpar = T)
  )
  
  # Default modifications
  # .. Remove inits
  parameters_df <- filter(parameters_df, !str_detect(name0, "^init_"))
  # .. Set estscale to "N" for offsets
  if (FLAGguessEstScale) 
    parameters_df <- mutate(parameters_df, estscale = case_when(str_detect(name, "^offset_") ~ "N", TRUE ~ estscale))
  
  parameters_df
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
cf_parameters_df_duplicate_inits <- function(parameters_df) {
  parnames <- parameters_df$name[parameters_df$FLAGinitpar]
  parnames_new <- c(parnames, paste0("init_", parnames))
  x <- unique(parnames)[1]
  for (x in unique(parnames)){
    parnames_new_x   <- parnames_new[parnames == x]
    
    parameters_df_new    <- filter(parameters_df[setdiff(names(parameters_df), "condition")], name == x) %>% 
      mutate(name = list(tibble(name  = parnames_new_x, condition = conditions))) %>% unnest(name)
    parameters_df <- bind_rows(parameters_df, parameters_df_new)
  }
  parameters_df
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
  # [] keep or drop original parameters? => keep
  # [] condition is now always only the latest supplied conditions supplied to this parameter...
  parnames_new <- outer(parnames, conditions, paste, sep = "_")
  x <- unique(parnames)[1]
  for (x in unique(parnames)){
    parnames_new_x   <- parnames_new[parnames == x]
    
    parameters_df_new    <- filter(parameters_df[setdiff(names(parameters_df), "condition")], name == x) %>% 
      mutate(name = list(tibble(name  = parnames_new_x, condition = conditions))) %>% unnest(name)
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
cf_parameters_df_merge_values <- function(pars_df_into, from, scale = c("outer", "inner")) {
  
  scale <- scale[1]
  scalenm <- c("outer" = "outer_value", "inner" = "value")
  scalenm <- scalenm[scale]
  if (is.vector(from)|"parvec" %in% class(from)){
    from <- unclass(from)
    from_dt <- data.table(name = names(from))
    from_dt[,scalenm[scale] := from[1:length(from)]]
  }
  
  pars_df_into <- data.table(pars_df_into)
  nm_dt <- copy(pars_df_into[,.(name)])
  
  pold <- pars_df_into[!from_dt, on = "name"]
  pnew <- copy(pars_df_into)[,(scalenm):=NULL][from_dt, on = "name"]
  p <- rbindlist(list(pold, pnew), fill = TRUE, use.names = TRUE)[nm_dt, on = "name"]
  
  if (scale == "outer") p[,`:=`(value = case_when(estscale == "L"~ exp(outer_value), TRUE ~ outer_value))]
  if (scale == "inner") p[,`:=`(outer_value = case_when(estscale == "L"~ log(value), TRUE ~ value))]
  
  p
}



#' Make a column of an est.grid condition specific and augment est.vec
#' 
#' new parameters will be named parname_"condition_column"
#' 
#' @param parname character(1L) denoting a column in the est.grid
#' @param conditions character vector of conditions
#' @param FLAGdummifyOtherConds replace the other parameters by "dummy". Dummy will have value c(dummy = 1)
#' @param est.grid 
#' @param parameters_est_df 
#' @param condition_column 
#' @param condition.grid if this is supplied, condition_column can also refer to a column in there
#' 
#' @export 
cf_make_condition_specific <- function(est.grid, parameters_est_df, parname, conditions, condition_column = "ID", FLAGdummifyOtherConds = FALSE,
                                       condition.grid = NULL){
  
  FLAGaugment_est.grid <- FALSE
  if (!is.null(condition.grid) & !condition_column %in% names(est.grid)){
    FLAGaugment_est.grid <- TRUE
    est.grid <- merge(est.grid, condition.grid[c("ID", "condition", condition_column)])
    }
  
  condition_indices <- which(est.grid[[condition_column]] %in% conditions)
  
  parnames <- est.grid[condition_indices, parname, drop = TRUE]
  est.grid[condition_indices, parname] <- paste0(est.grid[condition_indices, parname], "_", est.grid[condition_indices, condition_column])
  parnames_new <- est.grid[condition_indices, parname, drop = TRUE]
  
  .x <- unique(parnames)[1]
  for (.x in unique(parnames)){
    parnames_new_.x   <- parnames_new[parnames == .x]
    est.vec_df_new    <- filter(parameters_est_df, name == .x) %>% mutate(name = list(parnames_new_.x)) %>% unnest(name) %>% mutate(FLAGindividualized = TRUE)
    parameters_est_df <- bind_rows(parameters_est_df, est.vec_df_new)
  }
  
  if (FLAGdummifyOtherConds) {
    condition_indices <- !(est.grid[[condition_column]] %in% conditions)
    parnames <- unique(est.grid[[parname]])
    est.grid[condition_indices, parname] <- "dummy"
    parnames <- unique(est.grid[[parname]])
  }
  
  # remove the column again
  if (FLAGaugment_est.grid)
    est.grid[[condition_column]] <- NULL
  
  parameters_est_df <- unique(parameters_est_df)
  
  return(list(est.grid = est.grid, est.vec_df = parameters_est_df))
}



#' @export
unclass_parvec <- function(x) {setNames(unclass(x)[1:length(x)], names(x))}





