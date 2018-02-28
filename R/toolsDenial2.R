#' Import a model from the D2D definition files to dMod
#'
#' @param setup_file A script which loads the model and data
#' @param model_def_file 
#' @param data_files Character vector with file paths
#' @param readme_file 
#'
#' @return
#' @export
#'
#' @examples
d2d2r <- function(setup_file = NULL, 
                  model_def_file = NULL, 
                  data_files = NULL, 
                  readme_file = NULL) {
  
  if (is.null(setup_file) & (is.null(model_def_file)| is.null(data_files))) stop("If setup_file is not provided, model_def_file and data_files must be provided")
  
  if (!is.null(setup_file)) {
    modelpath <- setup_file %>% str_split(.Platform$file.sep, simplify = T) %>% {.[1:(length(.)-1)]} %>% as.list %>% do.call(file.path,.)
  } else {
    modelpath <- model_def_file %>% str_split(.Platform$file.sep, simplify = T) %>% {.[1:(length(.)-2)]} %>% as.list %>% do.call(file.path,.)
  }
  
  if (!is.null(setup_file)) {
    model_def_file <- readLines(setup_file) %>% 
    {.[!str_detect(.,"^%")]} %>% #commented lines
    {.[str_detect(.,"arLoadModel")]} %>% # only lines loading the model (should be only 1)
      str_replace_all(c("arLoadModel\\(" = "", "\\)" = "", "'" = "", ";.*$" = "")) %>% #extract only the name of the model.def
      paste0("Models/",. ,".def") %>% # make file path out of it
      list(modelpath,.) %>% 
      do.call(file.path,.) 
  }
  
  model_def <- read_def_content(model_def_file)
  
  if (!is.null(setup_file)) data_files <- readLines(setup_file) %>% 
  {.[!str_detect(.,"^%")]} %>% #commented lines
  {.[str_detect(.,"arLoadData")]} %>% # only lines loading the model (should be only 1)
    str_replace_all(c("arLoadData\\(" = "", "\\)" = "", "'" = "", ";.*$" = "")) %>% #extract only the name of the model.def
    str_split(",", simplify = T)  
  

  
  data_def_files <- data_files %>% 
  {paste0("Data/",.[,1] , ".def")} %>% # make file path out of it
    list(modelpath,.) %>% 
    do.call(file.path,.)
  
  data_sheet_files <- data_files %>% 
  {paste0("Data/",.[,1] , ".", .[,3])} %>% # make file path out of it
    list(modelpath,.) %>% 
    do.call(file.path,.) 
  
  files_exist <- file.exists(data_def_files)&file.exists(data_sheet_files)
  data_files <- data_files[files_exist,]
  data_def_files <- data_def_files[files_exist]
  data_sheet_files <- data_sheet_files[files_exist]
  
  data_defs <- data_def_files %>% 
    lapply(. %>% read_def_content)
  
  data_sheets <- lapply(seq_along(data_sheet_files), function(i) {
    if(data_files[i,3] == "csv") {
      return(read.csv(data_sheet_files[i], stringsAsFactors = F))
    } else {
      return(readxl::read_excel(data_sheet_files[i]))
    }
  })
  
  
  # Build the prediction function
  
  # # 1. Read inputs and incorporate them # -> Moved to get_reactions
  # inputs <- get_inputs(model_def)
  # inputs_data <- data_defs %>% lapply(. %>% get_inputs)
  # if (!is.null(inputs)|!is.null(do.call(c, inputs_data))) warning("Inputs not yet implemented completely")
  
  # 2. Read the reactions/odes
  reactions <- get_reactions_or_odes(model_def)
  if(is.eqnlist(reactions)) {
    f <-  as.eqnvec(reactions)
  } else {
    f <-  reactions
  }
  

  
  
  
  
  # 3. Build the odemodel/load it, if it exists already
  odemodelname <- str_replace_all(model_def_file, structure(c("", "", ""), names = c(modelpath, "/", "\\.def")))
  odemodel_rda <- str_replace_all(model_def_file, structure(c(".rda"), names = c("\\.def$")))
  if (!file.exists(odemodel_rda)) {
    myodemodel <-  odemodel(f, modelname = odemodelname)
    save(myodemodel, file = odemodel_rda)  
  } else {
    load(odemodel_rda)
  }
  x <-  Xs(odemodel = myodemodel)
  loadDLL(x)
  
  # Observables
  # 1. Read observables in model_def and the data_defs
  observables <-  get_observables(model_def)
  obs_data_def <- data_defs %>% lapply(. %>% get_observables)
  
  # 2. For each data_def, replace the specific obervables
  observables <- obs_data_def %>% lapply(. %>% replace(observables, names(.),.) %>% as.eqnvec)
  
  
  # Error models
  # 1. Read errors in model_def and the data_defs
  errors <-  get_errors(model_def)
  errors_data_defs <- data_defs %>% lapply(. %>% get_errors)
  
  # 2. For each data_def, replace the specific errors
  errors <- errors_data_defs %>% lapply(. %>% replace(errors, names(.),.) %>% as.eqnvec)
  
  
  # Conditions and datalist generation
  
  # Hierarchy of conditions: data_xls overwrites data_def overwrites model_def
  
  # Detailed explanation where conditions come from
  # a) In the model_def, a default condition is specified, which applies to all data sets, if in the data_def and the data_xls there is nothing to overwrite it.
    # Example: init_par_pX <- 0
  # Each data set possibly gives rise to different conditions
  # b) In the data_def, there can be specifications which apply to the whole data_sheet
    # Example: init_par_pY <- 0
  # c) In the data_def, "random" parameters can be specified, which gives rise to different parameters which are estimated separately
    # Example: 
      # In data_def or model_def, there is a parameter called something like "par_nExpID"
      # In data_def: RANDOM\n nExpID
      # In data_xls: .$nExpID: c(1,1,1,1,1,2,2,2,2,)
      # Then this gives rise to two parameters "par_nExpID1" and "par_nExpID2" which both map to par_nExpID
  # d) In the data_xls, parameters can be supplied directly as columns with column_name = parameter_name
  
 # Steps 1 to 3
  trafos <- lapply(seq_along(observables), function(i) {
    # Foreplay
    model_cond <- get_conditions(model_def)
    data_cond <- get_conditions(data_defs[[i]])
    
    # construct the trafo
    trafo <- c(names(f), # 1. Make a identity-trafo in which the transformed parameters will be replaced successively
               getSymbols(f),
               getSymbols(observables[[i]]),
               getSymbols(errors[[i]])
    ) %>% 
      unique %>% 
      structure(.,names = .) %>% 
      replace(.,names(model_cond), model_cond) %>%  # 2. Replace parameters by the parameters given in model_def$conditions
      replace(.,names(data_cond), data_cond)  # 3. Replace parameters by the parameters given in data_def$conditions
    
    return(trafo)
  })

  # 4. Read data_xls, append file name as its own column to distinguish between different data_defs, turn data sheet objects into datalists. 
  # This gives the condition.grid which contains all conditions specified by the data sets.
  
  # It happened already that some observables were commented out in the data_def, but were still present in the data sheet.
  # Kick out all columns which don't define any observables, parameters or time or RANDOM_ids
  random_ids <- get_random(model_def) %>% c(lapply(data_defs,. %>% get_random) %>% do.call(c,.)) %>% unique
  
  mydatalist <-  NULL
  mydatalist <- lapply(seq_along(data_sheets), function(i) {
    
    observable_names <- names(observables[[i]])
    mytrafo <- trafos[[i]]
    mynames <- names(data_sheets[[i]]) %>% str_replace("^init_", "")
    
    undefined_cols <- mynames[!(mynames%in%c(observable_names, getSymbols(c(mytrafo, names(mytrafo))), "time")) &
                                !(mynames %>% sapply(. %>% str_detect(random_ids) %>% any))]
    if (length(undefined_cols)>0) warning("Columns ", paste(undefined_cols, collapse = "\n"), " were kicked out from data sheet ", i, ": ", data_files[i,1], "\n\n")
    
    if (any(str_detect(names(data_sheets[[i]]), "_std"))) warning("std supplied in data sheet not yet implemented.")
    
    data_sheets[[i]] %>% 
    {.[!(names(.) %in% undefined_cols)]} %>% #kick out undefined cols
      tidyr::gather_(., key_col = "name", value = "value", gather_cols = names(observables[[i]])[names(observables[[i]]) %in% names(.)], na.rm = T) %>% # tidy the data into the dMod-style, name, time, value, sigma, (additional cols which define the condition)
      cbind(sigma = NA, data_file = paste0(i)) %>% 
      {names(.) <- names(.) %>% str_replace("^init_", "")
      .}
  }) %>% 
    plyr::join_all(., type = "full") %>% 
    as.datalist()

  cg <- mydatalist %>% attr("condition.grid")
  
  # 5. Insert parameter values given in the data sheet into the parameter trafos
  # Make a comprehensive list of parameter trafos for all conditions specified in the condition.grid
  cg_which_random <- names(cg) %>% sapply(. %>% str_detect(random_ids)%>% any)
  cg_random <- cg[cg_which_random]
  cg_pars <- cg[!(cg_which_random|str_detect(names(cg),"data_file"))]
 
  trafo_list <-  list()
  trafo_list <- lapply(1:nrow(cg), function(i) {
    
    mytrafo <- trafos[[as.integer(cg[["data_file"]][i])]]
    mytrafo_names <- names(mytrafo) # I need that later, because str_ somehow kills the names
    
    # Deal with RANDOM pars
    # They are most likely to appear in sd and offset pars. 
    random_id_values <- cg_random[i,,drop = F] %>% unlist
    
    # # Kill (ie set to 1) all "random" pars which don't contain an observed observable in the respective experiment. This is old, but I'll leave it in in just in case...
    # observed <- mydatalist[[i]]$name %>% unique
    # mytrafo[(!sapply(mytrafo, . %>% str_detect(observed) %>% any)) & 
              # sapply(mytrafo, . %>% str_detect(names(random_id_values)) %>% any)] <- "1"
    
    # Insert Random-ID values
    # # The RANDOM-ID can be NA, but then it shouldn't appear in the trafo.
    if(any(is.na(random_id_values)) & mytrafo %>% sapply(. %>% str_detect(names(cg_random))) %>% any) warning("random ids are NA.")
    what <- paste0("([\b_]*)(?:", names(random_id_values),")([\b_]*)")
    by <- paste0("\\1", names(random_id_values),  random_id_values)
    replace_vec <- structure(by, names = what)
    if(length(replace_vec)>0) {
      mytrafo <-
        mytrafo %>% str_replace_all(replace_vec) %>% {names(.) <- mytrafo_names;.}
    }
    # str_replace("abcdefg", "(ab)(?:cd)", "\\1hij") # to understand the upper regex
    
    
    # Deal with parameter values specified in the data sheet
    # Because of having multiple data sheets, some of these parameters can be NA, ie not specified in this data sheet.
    mycg_pars <- cg_pars[i,,drop = F] %>% unlist()
    # There are pars which are already replaced by a numeric value, these cannot be replaced by str_replace.
    is_already_set <- names(mycg_pars) %>% sapply(. %>% paste0("\\b", ., "\\b") %>% str_detect(mytrafo,.) %>% any %>% {!.} )
    if(any((!is_already_set)&(!is.na(mycg_pars)))) {
      what <- paste0("\\b",names(mycg_pars)[(!is_already_set)&(!is.na(mycg_pars))], "\\b")
      by <- as.character(mycg_pars)[(!is_already_set)&(!is.na(mycg_pars))]
      replace_vec <- structure(by, names = what)
      if (length(replace_vec)>0) {
        mytrafo <- mytrafo %>% str_replace_all(replace_vec) %>% {names(.) <- mytrafo_names;.}
      } 
    }
    # Replace all remaining pars by accessing them directly
    mytrafo[names(mycg_pars[is_already_set&(!is.na(mycg_pars))])] <- mycg_pars[is_already_set&(!is.na(mycg_pars))]
    
    return(mytrafo)
  }) %>% structure(., names = rownames(cg))


  
  # ##############################################################################################
  # Compile both P and g
  # Make them loadable after saving
  # #######################################
  
  # 7. Define the parameter trafos for the different conditions
  p_rda <- str_replace_all(model_def_file, structure(c("_parfn.rda"), names = c("\\.def$")))
  if (!file.exists(p_rda)) {
  p_grid <- NULL
  for (i in 1:nrow(cg)) {
    print(i)
    p_grid <- p_grid + P(trafo_list[[i]], condition = names(trafo_list)[i])
  }
  save(p_grid, file = p_rda)
  } else {
    load(p_rda)
  }
  
  # 8. Define the observation functions for the different conditions. Group those by data file
  g_rda <- str_replace_all(model_def_file, structure(c("_obsfn.rda"), names = c("\\.def$")))
  if (!file.exists(g_rda)) {
    g <- NULL
    for (i in 1:nrow(cg)) {
      print(as.integer(cg[["data_file"]][i]))
      print(rownames(cg)[i])
      g <- g + Y(g = observables[[as.numeric(labels(cg[["data_file"]][i]))]], 
                  f = f, 
                  condition = rownames(cg)[i]
                  #,
                  # compile = T,
                  # modelname = paste0(odemodelname, rownames(cg)[i])
      )
    }    
    save(g, file = g_rda)
  } else {
    load(g_rda)
  }
  

  # 9. Define the errormodel functions
  error_model_rda <- str_replace_all(model_def_file, structure(c("_error_model.rda"), names = c("\\.def$")))
  if (!file.exists(error_model_rda)) {
    err <- NULL
    for (i in 1:nrow(cg)) {
      print(as.integer(cg[["data_file"]][i]))
      print(rownames(cg)[i])
      if (!is.null(errors[[as.numeric(labels(cg[["data_file"]][i]))]])){
        err <- err + Y(g = errors[[as.numeric(labels(cg[["data_file"]][i]))]], 
                        f = f, 
                        condition = rownames(cg)[i]
                        #,
                        # compile = T,
                        # modelname = paste0(odemodelname, rownames(cg)[i])
        )
      }
    }    
    save(err, file = error_model_rda)
  } else {
    load(error_model_rda)
  }
  

  
  
mypars <- getParameters(g*x*p_grid)
# mypars <- getParameters(x*p_grid)

p_log <- P(trafo = structure(paste0("10^(log10", mypars, ")"), names = mypars))
pouter <- rnorm(length(mypars)) %>% set_names(paste0("log10", mypars), sd = 0.5)
obj <- normL2(mydatalist, (g*x*p_grid*p_log), errmodel = err)  + constraintL2(structure(rep(0, length(pouter)), names = names(pouter)))


# pars <- runif(length(mypars),0.0001,1) %>% set_names(mypars)
obj(pouter)


mypred <- (g*x*p_grid)(seq(0,600,length.out = 100), pars, deriv = F)
# mypred <- (x*p_grid)(seq(0.001,100, length.out = 10), pars)

myfit <- trust(objfun = obj, pouter, rinit = 1, rmax = 5)



plotPrediction(mypred, name %in% names(observables[[1]]))
}

