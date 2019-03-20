

#' Choose the best out of several cut lenghts
#'
#' @param vec the vector you want to cut into pieces
#' @param possibilities the possible sizes of the slices in preferred order
#' @export
#'
#' @examples
#' vec <- letters
#' possibilities <- c(12,15,10)
#' optimal_cuts(vec, possibilities)
#' possibilities <- c(12,10,13)
#' optimal_cuts(vec, possibilities)
optimal_cuts <- function(vec, possibilities) {

  if (!length(vec))
    return(NULL)

  whichzero_or_max <- function(x) {
    if (any(x == 0))
      return(which(x == 0))
    return(which.max(x))
  }
  best <- length(vec) %>% `%%`(possibilities) %>% whichzero_or_max

  cut_interval(seq_along(vec), length = possibilities[best], labels = FALSE)
}


#' Title
#'
#' @param myparframe
#'
#' @return
#' @export
#'
#' @examples
get_parcolindices <- function(myparframe) {
  which(names(myparframe) %in% attr(myparframe, "para"))
  }

#' do Plots
#'
#' @param model a dMod.frame
#' @param pngname,height,width as in \code{png()}
#' @param FLAGdev.off TRUE: shut down png device at the ende, FALSE: don't shut it down
#' @param tol for stepDetect in parframe
#' @param FLAG_waterfalls plotValues, subsetted to various chi2 value-thresholds
#' @param FLAG_plotPars plotPars of steps 1-3, 1-5 and the largest step
#' @param FLAG_scatterall_step1 do a scatterplot of all parameters at step 1
#' @param FLAG_iterations histogram of the number of iterations
#' @param nm list with plotting helpers, such as a character vector of statenames or observables. their list entry must be called "states" and "observables"
#' @param FLAG_predictions_conds_states_observables
#' @param FLAG_predictions_states_conds
#' @param FLAG_predictions_observables_conds
#' @param FLAG_profiles
#'
#' @return
#' @export
#'
#' @examples
doPlots1 <- function(model,
                     # png options
                     pngname = "plots%03d.png",
                     height = 1000, width = 1200,
                     FLAGdev.off = TRUE,
                     # Parframes
                     tol = 1,
                     FLAG_waterfalls = TRUE,
                     FLAG_plotPars = TRUE,
                     FLAG_scatterall_step1 = FALSE,
                     FLAG_iterations = TRUE,
                     # Predictions
                     nm = NULL,
                     FLAG_predictions_conds_states_observables = TRUE,
                     FLAG_predictions_states_conds = TRUE,
                     FLAG_predictions_observables_conds = TRUE,
                     FLAG_profiles = TRUE
                     ){

  # ---------------------------------------------------------- #
  # 1 Initialize ----
  # ---------------------------------------------------------- #
  png(pngname, height = height, width = width)

  # ----------------------------------------------- #
  # .. 1. preliminary definitions  ----
  # ----------------------------------------------- #
  conditions <- model$conditions[[1]]

  myparframe <- model$parframes[[1]] %>% add_stepcolumn(tol = tol)
  ndata <- model$obj_data[[1]] %>% environment() %>% as.list() %>% .$data %>% .[conditions] %>% as.data.frame() %>% nrow
  assign("ndata", ndata, .GlobalEnv)
  parameters <- getParameters(myparframe)

  pred0 <- with(uh(model), prd(0:1, pars, fixed = fixed, deriv = FALSE, conditions = conditions))
  pred0_frame <- pred0 %>% wide2long() %>% filter(time == 0)

  vars_predicted <- pred0_frame %>% .$name %>% unique() %>% as.character()
  states <- intersect(nm$states, vars_predicted)
  observables <- intersect(nm$observables, vars_predicted)

  obs_list <- split(pred0_frame, pred0_frame$condition) %>% map(~intersect(.x$name, observables))


  prof_vars <- NULL
  if (length(model$profiles))
    prof_vars <- unique(model$profiles[[1]]$whichPar)

  cuts <- list(conditions = optimal_cuts(conditions, c(5,4,6,7)),
               vars_predicted = optimal_cuts(vars_predicted, c(12,15,10)),
               states = optimal_cuts(states, c(12,15,10)) ,
               observables = optimal_cuts(observables, c(12,15,10)) ,
               prof_vars = optimal_cuts(prof_vars, c(12,15,10)) ,
               parameters = optimal_cuts(parameters, c(20,15)))

  # ---------------------------------------------------------- #
  # 2 Parframes ----
  # ---------------------------------------------------------- #
  # ----------------------------------------------- #
  # .. 1 Waterfalls ----
  # ----------------------------------------------- #
  if (FLAG_waterfalls){
  p1 <- myparframe %>% plotValues() + ggtitle("all fits that did not end in an error")
  print(p1)
  p1 <- myparframe %>% plotValues(tol, value < quantile(value, 0.5)) + ggtitle("50% of fits that did not end in an error")
  print(p1)
  p1 <- myparframe %>% plotValues(tol, value < quantile(value, 0.1)) + ggtitle("10% of fits that did not end in an error")
  print(p1)
  p1 <- myparframe %>%  plotValues(tol, step <= 2) + ggtitle("First two steps")
  print(p1)
  p1 <- myparframe %>%  plotValues(tol, value < (min(value) + ndata))+ ggtitle("value < min(chi^2) + ndata")
  print(p1)
  }

  # ----------------------------------------------- #
  # .. 2 Pars ----
  # ----------------------------------------------- #

  if (FLAG_plotPars) {
    map(unique(cuts$parameters), function(.x) {
      parameters_ <- parameters[cuts$parameters == .x]
      parframe_ <- subsetparameters(myparframe, parameters_)
      p1 <- plotPars(parframe_, 1, step <= 3) + ggtitle("Parameters of step 1-3")
      print(p1)
    })
    map(unique(cuts$parameters), function(.x) {
      parameters_ <- parameters[cuts$parameters == .x]
      parframe_ <- subsetparameters(myparframe, parameters_)
      p1 <- plotPars(parframe_, 1, step <= 5) + ggtitle("Parameters of step 1-5")
      print(p1)
    })
    map(unique(cuts$parameters), function(.x) {
      parameters_ <- parameters[cuts$parameters == .x]
      parframe_ <- subsetparameters(myparframe, parameters_)
      p1 <- plotPars(parframe_, 1, stepsize == max(stepsize)) + ggtitle("Parameters of largest step")
      print(p1)
    })
  }

  if (FLAG_scatterall_step1){
  p1 <- myparframe %>%
    as.data.frame %>%
    filter(step == 1) %>%
    mutate(val = cut(value, breaks = quantile(value, c(0, 0.05,0.5,1)))) %>%
    GGally::ggpairs(
      mapping = aes(color = val),
      columns = get_parcolindices(myparframe)) +
    ggtitle("Parameter values of first step")
  print(p1)
  }

  if (FLAG_iterations){
  p1 <- myparframe %>%
    as.data.frame() %>%
    ggplot(aes(x = iterations, y = stat(count))) +
    geom_density() +
    ggtitle("Number of iterations")
  print(p1)
}


  # ---------------------------------------------------------- #
  # 3 Predictions ----
  # ---------------------------------------------------------- #

  # Scenarios
  # 1. many conditions
  # 2. many names, observables
  # 3. many steps



  # ----------------------------------------------- #
  # .. 2.  Plot step1 looped over conditions----
  # ----------------------------------------------- #
  if (FLAG_predictions_conds_states_observables){
  map(unique(cuts$conditions), function(cn_ind) {
    conditions_ <- conditions[cuts$conditions == cn_ind]
    map(unique(cuts$states), function(states_ind) {
      states_ <- states[cuts$states == states_ind]
        p1 <- plotCombined(model, 1, 1, condition %in% conditions_ & name %in% states_) +
          theme(legend.position = "bottom", legend.direction = "horizontal") +
          ggtitle("Step 1, internal states")
        print(p1)
      })

    obscuts <- Reduce(union, obs_list[conditions_]) %>% optimal_cuts(c(12,15,10))
    map(unique(obscuts), function(obs_ind) {
      observables_ <- observables[obscuts == obs_ind]
      p1 <- plotCombined(model, 1, 1, condition %in% conditions_ & name %in% observables_) +
        theme(legend.position = "bottom", legend.direction = "horizontal") +
        ggtitle("Step 1, observables")
      print(p1)
    })
  })
  }



  # ---------------------------------------------------------- #
  # 4 Profiles ----
  # ---------------------------------------------------------- #

  # ----------------------------------------------- #
  # .. 2 Plot profiles ----
  # ----------------------------------------------- #
  if (FLAG_profiles & length(model$profiles)) {
    map(unique(cuts$prof_vars), function(.x) {
      prof_vars_ <- prof_vars[cuts$prof_vars == .x]
      p1 <- plotProfile(model, 1, whichPar %in% prof_vars_) +
        # theme(legend.position = "bottom", legend.direction = "horizontal") +
        ggtitle("Profiles")
      print(p1)
    })
  }



  # ---------------------------------------------------------- #
  # Analysis of single fits ----
  # ---------------------------------------------------------- #

  # # ---------------------------------------------------------- #
  # # 2 Look at one Fit from fitlist ----#
  # # ---------------------------------------------------------- #
  # myparframe %>% head
  # myfit <- model$fits[[1]][[myparframe[1]$index]] # This is the best fit of the mstrust
  #
  # p1 <- plotCombined(model)
  # print(p1)
  #
  #
  # p1 <- myfit$argpath %>%
  #   as.data.frame() %>%
  #   setNames(names(myfit$argument)) %>%
  #   rowid_to_column() %>%
  #   gather(name, value, -1) %>%
  #   ggplot(aes(rowid, value, color = name)) +
  #   geom_line() +
  #   # coord_cartesian(xlim = c(800, 850)) +
  #   geom_blank() +
  #   ggtitle("argpath of best fit")
  # print(p1)
  #
  # len_accepted <- length(myfit$valpath)
  # mydf <- myfit[map_int(myfit, length) == len_accepted] %>%
  #   as.data.frame() %>%
  #   rowid_to_column() %>%
  #   mutate(preddiff_abs = abs(preddiff),
  #          preddiff_sign = sign(preddiff)) %>%
  #   select(-steptype, -preddiff, -accept) %>%
  #   gather(name, value, -1) %>%
  #   mutate(value = log10(value))
  #
  # pardf <- myfit$argpath %>%
  #   as.data.frame() %>%
  #   setNames(names(myfit$argument)) %>%
  #   rowid_to_column() %>%
  #   gather(parameter, value, -1) %>%
  #   mutate(name = "parameter")
  #
  # mydf <- bind_rows(mydf, pardf)
  #
  # p1 <- mydf %>%
  #   ggplot(aes(rowid, value)) +
  #   geom_line(aes(color = parameter))+
  #   facet_wrap(~name, scales = "free_y")  +
  #   ggtitle("fit statistics of best fit")
  # print(p1)
  #
  #
  # # plotMulti
  # myframe <- myfit$argpath %>%
  #   as.data.frame() %>%
  #   setNames(names(myfit$argument)) %>%
  #   rowid_to_column("index") %>%
  #   parframe(.,
  #            parameters = setdiff(colnames(.), "index"),
  #            metanames = "index")
  #
  # p1 <- myframe %>%
  #   plotMulti(NULL, model) +
  #   facet_wrap(~name, scales = "free") +
  #   ggtitle("multiplot of best fit")
  # print(p1)
  # # ---------------------------------------------------------- #
  # # 3 Look at one Fit from fitlist with many iterations ----#
  # # ---------------------------------------------------------- #
  # myindex <- myparframe %>% as.data.frame() %>% filter(step == 1) %>% filter(iterations == max(iterations)) %>% .$index %>% .[1]
  # ranking <- myparframe %>% as.data.frame() %>% rowid_to_column() %>% filter(index == myindex) %>% .$rowid
  # myfit <- model$fits[[1]][[myindex]] # This is the best fit of the mstrust
  #
  # p1 <- plotCombined(model,1, ranking)
  # print(p1)
  #
  #
  # p1 <- myfit$argpath %>%
  #   as.data.frame() %>%
  #   setNames(names(myfit$argument)) %>%
  #   rowid_to_column() %>%
  #   gather(name, value, -1) %>%
  #   ggplot(aes(rowid, value, color = name)) +
  #   geom_line() +
  #   # coord_cartesian(xlim = c(800, 850)) +
  #   geom_blank() +
  #   ggtitle("argpath of fit with many iterations")
  # print(p1)
  #
  # len_accepted <- length(myfit$valpath)
  # mydf <- myfit[map_int(myfit, length) == len_accepted] %>%
  #   as.data.frame() %>%
  #   rowid_to_column() %>%
  #   mutate(preddiff_abs = abs(preddiff),
  #          preddiff_sign = sign(preddiff)) %>%
  #   select(-steptype, -preddiff, -accept) %>%
  #   gather(name, value, -1) %>%
  #   mutate(value = log10(value))
  #
  # pardf <- myfit$argpath %>%
  #   as.data.frame() %>%
  #   setNames(names(myfit$argument)) %>%
  #   rowid_to_column() %>%
  #   gather(parameter, value, -1) %>%
  #   mutate(name = "parameter")
  #
  # mydf <- bind_rows(mydf, pardf)
  #
  # p1 <- mydf %>%
  #   ggplot(aes(rowid, value)) +
  #   geom_line(aes(color = parameter))+
  #   facet_wrap(~name, scales = "free_y")  +
  #   ggtitle("fit statistics of fit with many iterations")
  # print(p1)
  #
  #
  # # plotMulti
  # myframe <- myfit$argpath %>%
  #   as.data.frame() %>%
  #   setNames(names(myfit$argument)) %>%
  #   rowid_to_column("index") %>%
  #   filter(((index %% round(max(index)/50)) == 1) | (index == max(index))) %>%
  #   parframe(.,
  #            parameters = setdiff(colnames(.), "index"),
  #            metanames = "index")
  #
  # p1 <- myframe %>%
  #   plotMulti(NULL, model) +
  #   facet_wrap(~name, scales = "free") +
  #   ggtitle("multiplot of fit with many iterations")
  # print(p1)
  #
  #
  # # ---------------------------------------------------------- #
  # # Analyze fits that got lost----#
  # # ---------------------------------------------------------- #
  # fits <- model$fits[[1]]
  # parnames <- names(model$pars[[1]])
  # ftz <- imap(fits, function(.x,.y) {
  #   value <-  .x$value
  #   has_error <- FALSE
  #   if (is.null(value)){
  #     value <- NA
  #     has_error <- TRUE
  #   }
  #   argpath_last <- .x$argpath
  #   if(!is.null(argpath_last))
  #     argpath_last <- argpath_last[nrow(argpath_last),]
  #   stepnorm_last <- .x$stepnorm
  #   if (is.null(stepnorm_last))
  #     stepnorm_last <- NA
  #   stepnorm_last <- stepnorm_last[length(stepnorm_last)]
  #
  #   tibble(index = .y,
  #          value = value,
  #          has_error = has_error,
  #          iterations = .x$iterations,
  #          converged = .x$converged,
  #          stepnorm_last = stepnorm_last,
  #          argument = list(.x$argument),
  #          parinit = list(.x$parinit),
  #          argpath_last =list(argpath_last))
  # }) %>% bind_rows
  #
  # mydf <- ftz %>%
  #   filter(is.na(value), iterations > 0, stepnorm_last < 20)
  # if (nrow(mydf) > 0){
  #   p1 <- mydf %>%
  #     ggplot(aes(stepnorm_last)) +
  #     geom_density() +
  #     ggtitle("last stepsize of lost fits")
  #   print(p1)
  # }
  #
  # mydf_ <- ftz %>%
  #   filter(is.na(value), iterations == 0)
  # if (nrow(mydf) > 0){
  #   p1 <-  mydf%>%
  #     .$argument %>%
  #     do.call(rbind,.) %>%
  #     as.data.frame() %>%
  #     setNames(parnames) %>%
  #     {.} %T>% {assign("n", nrow(.), .GlobalEnv)} %>%
  #     GGally::ggpairs() + ggtitle("startpars that got lost immediately", paste0("n = ",n))
  #   print(p1)
  # }
  #
  #
  # mydf <- ftz %>%
  #   filter(is.na(value), iterations > 0)
  # if(nrow(mydf)>0){
  #   p1 <- mydf %>%
  #     .$argument %>%
  #     do.call(rbind,.) %>%
  #     as.data.frame() %>%
  #     filter_all(all_vars(abs(.)<50)) %>%
  #     setNames(parnames) %>%
  #     {.} %T>% {assign("n", nrow(.), .GlobalEnv)} %>%
  #     GGally::ggpairs() + ggtitle("last arguments where it failed after some iterations", paste0("n = ",n))
  #   print(p1)
  # }
  #
  # # ftz %>%
  # #   filter(is.na(value)) %>%
  # #   .$argument %>%
  # #   do.call(rbind,.) %>%
  # #   as.data.frame() %>%
  # #   parframe()
  #
  # data.frame(file = filename,
  #            plotnames = plotnames,
  #            n = length(fits),
  #            nconverged = sum(ftz$converged),
  #            nlost = sum(is.na(ftz$value)),
  #            nlost_at_start = sum(ftz$iterations == 0),
  #            nstep1 = steps[2]-1) %>%
  #   write_csv(str_replace_all(plotnames, "%03d.png", ".csv"))


  # ---------------------------------------------------------- #
  # Exit ----
  # ---------------------------------------------------------- #

  if (FLAGdev.off){
    dev.off()
  } else {
    message("png-device still active")
  }

  return()
}
