#' Read the *.def file an prepare it for later use
#'
#' The function determines automatically if it's a data.def or a model.def file by checking if any of the headings REACTIONS, ODES or DERIVED are given. These are unique to model.def files.
#' 
#' @param connection The path to the file
#'
#' @return A data.frame with columns "def_content" and "section". "def_content" are the lines present in "section"
#' @export
#'
#' @examples
read_def_content <- function(connection) {
  def_content <- readLines(connection)
  # remove white lines, containing nothing or only spaces, tabs...
  def_content <- str_trim(def_content)
  def_content <- def_content[-grep("$[:space:]*^", def_content)]
  
  # clean up def_content: remove double spaces, space-tab, tab-space combinations, escaped quotation marks
  def_content <- str_replace_all(def_content, "\t", " ")
  def_content <- str_replace_all(def_content, " * ", " ")
  def_content <- str_replace_all(def_content, "\\\"", "")
  
  # remove commented lines and comments
  def_content <- def_content[!str_detect(def_content, "^//")]
  def_content <- def_content %>% str_replace_all(c("//.*$" = ""))
  
  
  # Deal with PREDICTOR-DOSERESPONSE parameter
  # Split this line into c("DOSERESPONSE", "parameter", "PREDICTOR"), take this line out of model_def and 
  # shove this vector in
  pred_dr_line_index <- str_detect(def_content, "PREDICTOR-DOSERESPONSE")
  if(any(pred_dr_line_index)) {
    pred_dr_line_index <- which(pred_dr_line_index)
    pred_dr <- str_split(def_content[pred_dr_line_index], " ", simplify = TRUE) 
    pred_dr <- c("DOSERESPONSE", pred_dr[2], "PREDICTOR")
    
    def_content <- c(def_content[1:(pred_dr_line_index-1)],
                     pred_dr,
                     def_content[(pred_dr_line_index+1):length(def_content)])
  }
  
  
  # section headings
  model_def_section_headings <- c("DESCRIPTION", "PREDICTOR", "COMPARTMENTS", "STATES", "INPUTS", "REACTIONS", "ODES", "DERIVED", "OBSERVABLES", "ERRORS", "SUBSTITUTIONS", "CONDITIONS")
  data_def_section_headings <- c("DESCRIPTION", "PREDICTOR", "PREDICTOR-DOSERESPONSE", "DOSERESPONSE", # DOSERESPONSE is not an original one, but derived from PREDICTOR-DOSERESPONSE
                                 "INPUTS", "OBSERVABLES", "ERRORS", "SUBSTITUTIONS", "CONDITIONS", "RANDOM")
  undescribed_section_headings <- c("INVARIANTS") # These are not described in the D2D-wiki
  all_section_headings <- union(union(x = model_def_section_headings, y = data_def_section_headings), undescribed_section_headings)

  # determine if it's a data.def or a model.def
  is_data_def <- !(any(c("REACTIONS", "ODES", "DERIVED") %in% def_content))
  
  # Determine which line belongs to which section
  section_headings <- def_content[def_content %in% all_section_headings]
  section_numbers <- cumsum(def_content %in% all_section_headings)
  
  # remove section heading lines
  section_numbers <- section_numbers[!def_content %in% all_section_headings]
  def_content <- def_content[!def_content %in% all_section_headings]
  
  # make data.frame where section is its own column to access the lines belonging to the sections are accessed easily
  def_content <- data.frame(def_content = def_content, section = section_headings[section_numbers], stringsAsFactors = F) 
  attr(def_content, "is_data_def") <- is_data_def
  
  return(def_content)
}


#' Read DESCTRIPTION
#'
#' @param def_content The def_content data.frame returned by read_def_content
#'
#' @return
#' @export
#'
#' @examples
get_description <- function(def_content) {
  
  description <- NULL
  if (any(def_content[["section"]] == "DESCRIPTION")) {
    warning("get_description not yet implemented")
  }
  return(description)
}




#' Read PREDICTOR
#' 
#' The predictor in d2d is often called "t", whereas in dMod it is called "time".
#' Therefore, in input functions, where "t" is explicitly called, the d2d-predictor has to be replaced by the dMod-predictor.
#'
#' @param def_content The def_content data.frame returned by read_def_content
#'
#' @return Character vector with regex names to be used by str_replace(). 
#' @export
#'
#' @examples
get_predictor <- function(def_content) {
  predictor <- NULL
  
  if (any(def_content[["section"]] == "PREDICTOR")) {
    if (length(def_content[["def_content"]][def_content[["section"]]=="PREDICTOR"])>1) {
      warning("More than one predictor. Does this make sense?")
    }
    predictor <- def_content[["def_content"]][def_content[["section"]]=="PREDICTOR"] %>% str_split(" ", simplify = T)
    # generate a named vector to use str_replace in places where the predictor is used to replace it by "time", the standard predictor of dMod.
    predictor <- structure("time", names = paste("\\b", predictor[1], "\\b"))
  }
  
  return(predictor)
}

#' Read DOSERESPONSE
#' 
#' Basically copied from get_predictor. Don't know if it's actually needed or what it's cool for.
#'
#' @param def_content The def_content data.frame returned by read_def_content
#'
#' @return Character vector with regex names to be used by str_replace(). 
#' @export
#'
#' @examples
get_doseresponse <- function(def_content) {
  dose_response_par <- NULL
  
  if (any(def_content[["section"]] == "DOSERESPONSE")) {
    dose_response_par <- def_content[["def_content"]][def_content[["section"]]=="DOSERESPONSE"] 
  }
  
  return(dose_response_par)
}

#' Read COMPARTMENTS
#'
#' Maybe the return value should be just an eqnvec vector c(name = eqn).
#'
#' @param def_content The def_content data.frame returned by read_def_content
#'
#' @return data.frame with columns c("name", "unit_type", "unit", "unit_plot_name", "eqn")
#' @export
#'
#' @examples
get_compartments <- function(def_content) {
  compartments <- NULL
  
  if (any(def_content[["section"]] == "COMPARTMENTS")) {
    
    
    compartments <- def_content[["def_content"]][def_content[["section"]]=="COMPARTMENTS"] %>% 
      str_split(" ", simplify = T) %>% 
      data.frame(stringsAsFactors = F)
    
    # Potential pitfall: Can columns 2-4 also be unspecified?
    if (length(compartments)<5) {compartments[5] <- paste0("vol_", compartments[1])}
    
    compartments_names <- c("name", "unit_type", "unit", "unit_plot_name", "eqn")
    names(compartments) <- compartments_names
    
  }
  
  return(compartments)
}




#' Read DERIVED
#' 
#' Maybe the return value should be just be an eqnvec c(name = eqn).
#'
#' @param def_content The def_content data.frame returned by read_def_content
#'
#' @return data.frame with columns c("name", "unit_type", "unit", "unit_plot_name", "eqn")
#' @export
#'
#' @examples
get_derived <- function(def_content) {
  derived <- NULL
  
  if (any(def_content[["section"]] == "DERIVED")) {
    derived <- def_content[["def_content"]][def_content[["section"]]=="DERIVED"]
    derived <- str_split(derived, " ", simplify = T)
    derived <- data.frame(derived, stringsAsFactors = F)
    
    # Columns 1-4 are always well defined
    # Column 5 might be spread across more columns. Paste these expressions back together
    derived_eqns <- derived[5:length(derived)] %>% apply(1, paste , collapse = " ")
    derived <- data.frame(derived[1:4], derived_eqns, stringsAsFactors = F)
    
    derived_names <- c("name", "unit_type", "unit", "unit_plot_name", "eqn")
    names(derived) <- derived_names[seq_along(derived)]
  }
  
  return(derived)
}




#' Read STATES
#' 
#' Maybe the return value could be just be a named vector c(name = compartment). 
#'
#' @param def_content The def_content data.frame returned by read_def_content
#'
#' @return data.frame with names c("name", "unit_type", "unit", "unit_plot_name", "compartment", "plot_flag", "plot_name", "is_positive")
#' @export
#'
#' @examples
get_states <- function(def_content) {
  states <- NULL
  
  if (any(def_content[["section"]] == "STATES")) {
    states <- def_content[["def_content"]][def_content[["section"]]=="STATES"]
    states <- str_split(states, " ", simplify = T)
    states <- data.frame(states, stringsAsFactors = F)
    
    states_df_names <- c("name", "unit_type", "unit", "unit_plot_name", "compartment", "plot_flag", "plot_name", "is_positive")
    names(states) <- states_df_names[seq_along(states)]
    
    states <- within(states, {
      if (exists("is_positive")) is_positive <- as.logical(as.numeric(is_positive))
      if (exists("plot_flag")) plot_flag <- as.logical(as.numeric(plot_flag))
    })
  }
  
  return(states)
}





#' Read INPUTS
#'
#' @param def_content The def_content data.frame returned by read_def_content
#'
#' @return NULL. It is not yet ready
#' @export
#'
#' @examples
get_inputs <- function(def_content) {
  inputs <- NULL
  
  if (any(def_content[["section"]] == "INPUTS")) {
    warning("get_inputs not yet complete")
    
    inputs <- def_content[["def_content"]][def_content[["section"]]=="INPUTS"]
    
    # debugging
    # inputs <- inputs[1]
    # inputs <- "alpha1 C ng/ml conc. step2(t, 0, time_delay, alpha_input,15.5,0) + step2(t,0,(time_delay*td),1,15.5,0)*(t-time_delay)*anstieg"
    
    inputs <- str_split(inputs, " ", simplify = T)
    inputs <- data.frame(inputs, stringsAsFactors = F)
    
    # the inputs in data_def only have two columns c("name", "eqn").
    # Insert dummy columns in this case
    if (attr(def_content, "is_data_def")) {
      inputs <- data.frame(inputs[1], NA, NA, NA, inputs[2:length(inputs)])
    }
    
    
    # Columns 1-4 are always well defined
    # Column 5 might be spread across more columns. Paste these expressions back together
    inputs_eqns <- inputs[5:length(inputs)] %>% apply(1, paste , collapse = " ")
    
    
    
    # parse the possible input functions
    d2d_input_functions <- c("step1(t, level1, switch_time, level2)",
                             "step2(t, level1, switch_time1, level2, switch_time2, level3)",
                             "smoothstep1(t, level1, switch_time, level2, smoothness)",
                             "smoothstep2(t, level1, switch_time1, level2, switch_time2, level3, smoothness)",
                             "bolus(t, amount, time_point, duration)",
                             "spline3(t, t_knot1, p_knot1, t_knot2, p_knot2, t_knot3, p_knot3, q_initial_slope_constraint, initial_slope)",
                             "spline4(t, t_knot1, p_knot1, t_knot2, p_knot2, t_knot3, p_knot3, t_knot4, p_knot4, q_initial_slope_constraint, initial_slope)",
                             "spline5(t, t_knot1, p_knot1, t_knot2, p_knot2, t_knot3, p_knot3, t_knot4, p_knot4, t_knot5, p_knot5, q_initial_slope_constraint, initial_slope)",
                             "spline10(t, t_knot1, p_knot1, t_knot2, p_knot2, t_knot3, p_knot3, t_knot4, p_knot4, t_knot5, p_knot5, t_knot6, p_knot6, t_knot7, p_knot7, t_knot8, p_knot8, t_knot9, p_knot9, t_knot10, p_knot10, q_initial_slope_constraint, initial_slope)",
                             "monospline3(t, t_knot1, p_knot1, t_knot2, p_knot2, t_knot3, p_knot3)",
                             "monospline4(t, t_knot1, t_knot1, p_knot1, t_knot2, p_knot2, t_knot3, p_knot3, t_knot4, p_knot4)",
                             "monospline5(t, t_knot1, p_knot1, t_knot2, p_knot2, t_knot3, p_knot3, t_knot4, p_knot4, t_knot5, p_knot5)",
                             "monospline10(t, t_knot1, p_knot1, t_knot2, p_knot2, t_knot3, p_knot3, t_knot4, p_knot4, t_knot5, p_knot5, t_knot6, p_knot6, t_knot7, p_knot7, t_knot8, p_knot8, t_knot9, p_knot9, t_knot10, p_knot10)",
                             # "inputspline(t, N, [t_knot1, t_knot2, ... t_knotN], [p_knot1, p_knot2, ... p_knotN])")
                             "inputspline(t, N, t_knot1, t_knot2, t_knotN, p_knot1, p_knot2, p_knotN)")
    
    d2d_input_functions <- d2d_input_functions %>% str_replace("\\)", "") %>% str_split("\\(", simplify = T) %>% apply(1, function(i) {
      # i[1]
      expand.grid(i[1],i[2] %>% str_split(",") %>% unlist, stringsAsFactors = F)
    }) %>% do.call(rbind,.)
    
    
    parsed <- getParseData(parse(text = inputs_eqns))
    
    d2d_input_functions_in_inputs <- parsed[(parsed[['text']] %in% unique(d2d_input_functions[[1]])),1:length(parsed)]
    if(nrow(d2d_input_functions_in_inputs)>0) stop("The symbolic d2d-input functions are not yet implemented")
    # arguments <- parsed[parsed[["token"]] %in% c("SYMBOL", "NUM_CONST"),1:length(parsed)]
    # 
    # # match arguments to their input functions
    # # find parent expressions of SYMBOL_FUNCTION_CALL, SYMBOL, NUM_CONST
    # input_function_parse_ids <- lapply(d2d_input_functions_in_inputs[["parent"]], function(myparent) {
    #   with(parsed, {parent[which(id == myparent)]})
    # })
    # 
    # argument_parents <- lapply(arguments[["parent"]], function(myparent) with(parsed, {
    #   parent[which(id == myparent)]
    # })
    # )
    # 
    # get_parent_chain <- function(parsed, start_row) {
    #   myparent <- myid <- c()
    #   myparent <- parsed[start_row, "parent"]
    #   myid <- parsed[start_row, "id"]
    #   i <- 1
    #   while(myparent[length(myparent)] != 0) {
    #     myparent[i+1] <- parsed[["parent"]][which(parsed[["id"]] == myparent[i])]%>% as.numeric()
    #     myid[i+1] <- parsed[["id"]][which(parsed[["id"]] == myparent[i])] %>% as.numeric()
    #     i <- i+1
    #   }
    #   return(data.frame(id = myid,parent = myparent))
    # }
    # 
    # 
    # argument_parent_chains <- lapply(arguments[["parent"]], function(myparent) {
    #   start_row <- which(parsed[["parent"]]== myparent) 
    #   get_parent_chain(parsed, start_row)
    # }) %>% structure(names = arguments[["text"]])
    # 
    # input_function_arguments <- lapply(input_function_parse_ids, function(id) { 
    #   # potential pitfall
    #   # noch nicht ganz fertig. wenn man als argument nicht nur einsymbolige tokens hat, wie z.b. step1(t,...), sondern step1((t+t0),...) kollabiert das ganze. ist vllt aber auch wumpe.
    #   sapply(argument_parent_chains, function(parent_chain) {
    #     any(parent_chain[["parent"]] %in% id)
    #   })
    # }) 
    # input_function_arguments <- lapply(seq_along(input_function_arguments), function(i) {input_function_arguments[[i]]*i}) %>% do.call("+",.) # this now tells you to which input function which argument belongs
    # 
    # 
    # # next: create a list of d2d <-> dMod forcings where you can translate parameters to dMod
    # # implement the remaining input-functions that are not yet given in forcingsSymb()
    # # replace the predictor "t" with "time"
    
    
    inputs <- structure(inputs[,5], names = inputs[,1]) %>% as.eqnvec()
    
  }
  return(inputs) # because not yet ready
}






#' Read REACTIONS
#'
#' @param def_content The def_content data.frame returned by read_def_content
#' @param derived get_derived(def_content) - data.frame with mandatory columns c("name", "eqn")
#' @param compartments get_compartments(def_content) - data.frame with mandatory columns c("name", "eqn")
#' @param states  get_states(def_content) - data.frame with mandatory columns c("name", "compartments")
#'
#' @return eqnlist of all reactions 
#' @export
#'
#' @examples
get_reactions_or_odes <- function(def_content, 
                                  derived = get_derived(def_content),
                                  compartments = get_compartments(def_content),
                                  states = get_states(def_content),
                                  inputs = get_inputs(def_content)) {
  myeqnlist <- eqnlist()
  f <- eqnvec()
  
  if (any(def_content[["section"]] == "REACTIONS")) {
    reactions <- def_content[["def_content"]][def_content[["section"]]=="REACTIONS"]
    
    # replace derived variables by their definitions
    
    # debugging 
    # reactions <- c( "A_state_2 + 2 B_state -> B_state CUSTOM p1*bA_state + A_state", "B_state -> C_state CUSTOM p2*B_state")
    # reactions <- reactions[1]
    # derived <- data.frame(name = c("A_state", "C_state"), eqn = c("dudoedel", "dupenner"), stringsAsFactors = F)
    # myderived <- structure(derived[["eqn"]], names = paste0('[\\A\\s\\*\\+\\"]', derived[["name"]], '[\\s\\*\\+\\"]'))
    
    # This would have been nicer with replaceSymbols, but since the line has not yet been split up into its columns, the strings cannot be parsed
    if (!is.null(derived)) {
      myderived <- structure(paste("(", derived[["eqn"]], ")"), names = paste0('\\b', derived[["name"]], '\\b'))
      reactions <- reactions %>% str_replace_all(myderived)
    }
    
    reactions <- str_split(reactions, " ")
    # debugging
    # r <- reactions[[1]]
    
    # Slice down each reaction into c("from", "to", "rate")
    reaction_list <- lapply(reactions, function(r) {
      # determine the position of the two big delimiters: c("->", "CUSTOM"/"MASSACTION") within the string
      to_position <- which(str_detect(r, "->"))
      reaction_type_position <- which(str_detect(r, "CUSTOM") | str_detect(r, "MASSACTION"))
      if (any(str_detect(r, "MASSACTION"))) warning( "Rate law given by MASSACTION, currently this is not supported by d2d2r.")
      
      # In d2d the stoichiometries can be given as "2 A -> A + B".
      # Make " 2*A -> A + B" out of it
      fix_stoichiometric_expression <- function(x) {
        stoichiometries <- suppressWarnings(!is.na(as.numeric(x)))
        x[stoichiometries] <- paste(x[stoichiometries], "*")
        paste(x, collapse = " ")
      }
      from <- fix_stoichiometric_expression(r[1:(to_position-1)])
      to <- fix_stoichiometric_expression(r[(to_position + 1):(reaction_type_position - 1)])
      
      # deal with synthesis or degradation. 
      # Since all whitespace has been removed, we can work with the positions within the string and obtain unabiguous results.
      # potential pitfall: the text--file might not be as well behaved as I want.
      # Check this with some examples
      if (to_position == 1) {
        from <- NA
      }
      if (reaction_type_position - to_position == 1) {
        to <- NA
      }
      
      rate <- r[(reaction_type_position + 1):length(r)] %>% paste(collapse = " ")
      
      return(c(from = from, to = to, rate = rate))
    })
    
    
    # debugging
    # i <- 1
    # reaction_list[[i]]
    
    
    # Incorporate the inputs
    if(!is.null(inputs)) reaction_list <- reaction_list %>% lapply(function(reaction) {
      myrate <- reaction %>% {.["rate"]} %>% str_replace_all(inputs)
      reaction["rate"] <- myrate
      return(reaction)
    })
    
    # construct the dMod object "eqnlist" from the list of reactions
    myeqnlist <- eqnlist()
    for(i in seq_along(reaction_list)) {
      myeqnlist <- addReaction(myeqnlist, from = reaction_list[[i]]["from"], to = reaction_list[[i]]["to"], rate = reaction_list[[i]]["rate"], description = as.character(i))
    }
    
    # incorporate different compartments
    if (!is.null(compartments)) {
      # make a vector mycompartments <- c(state=compartment)
      mycompartments <- structure(compartments[["eqn"]], names = compartments[["name"]])
      mycompartments <- structure(mycompartments[states[["compartment"]]], names = states[["name"]])
      # incorporate the mycompartments-vector into the eqnlist
      myeqnlist <- myeqnlist %>% 
        as.data.frame %>% 
        as.eqnlist(volumes = mycompartments) %>% # don't wonder, the volumes don't show up in the equations, when you print the eqnlist...
        {.}
    }
    return(myeqnlist)
    
  } else if (any(def_content[["section"]] == "ODES")) {
    warning("get_odes can't deal with compartments")
    
    # potential brainwise pitfall: here we don't need to consider "derived"
    f <- def_content[["def_content"]][def_content[["section"]]=="ODES"] %>% structure(names = states[["name"]]) %>% as.eqnvec()
    return(f)
    
  } else {
    return(NULL)
  }
}


#' Read OBSERVABLES
#' 
#' The data in the def_content file contains some columns which I'm not sure are needed in dMod
#' Therefore, this function just returns an eqnvec with the observables.
#' 
#' @param def_content The def_content data.frame returned by read_def_content
#' @param derived get_derived(def_content) - data.frame with mandatory columns c("name", "eqn")
#'
#' @return eqnvec with the observables
#' @export
#'
#' @examples
get_observables <- function(def_content, derived = get_derived(def_content)) {
  
  observables <- eqnvec()
  
  if (any(def_content[["section"]] == "OBSERVABLES")) {
    warning("'rescale' has not been implemented yet \n
            'log10' has not been implememented yet \n
          What happens if derived variables are directly used as observables?\n")
    # erst in g wichtig
    observables <- def_content[["def_content"]][def_content[["section"]]=="OBSERVABLES"]
    
    # replace derived variables by their definitions
    #
    if (!is.null(derived)) {
      myderived <- structure(paste("(", derived[["eqn"]], ")"), names = paste0('\\b', derived[["name"]], '\\b'))
      observables <- observables %>% str_replace_all(myderived)
    }
    
    observables <- str_split(observables, " ", simplify = T)
    observables <- data.frame(observables, stringsAsFactors = F)
    
    # Columns 1-6 are always well defined
    # Column 7 might be spread across more columns. Paste these expressions back together
    observables_eqns <- observables[7:length(observables)] %>% apply(1, paste , collapse = " ")
    observables <- data.frame(observables[1:6], observables_eqns, stringsAsFactors = F)
    
    
    observables_names <- c("name", "unit_type", "unit", "unit_plot_name", "rescale", "log10", "eqn")
    names(observables) <- observables_names[seq_along(observables)]
    
    observables <- structure(observables[["eqn"]], names = observables[["name"]]) %>% as.eqnvec()
  }
  
  return(observables)
}


#' Read ERRORS
#' 
#' The returned "errors" eqnvec can be used to make an observation function for the error-model
#' The "f"-argument to the call of Y can then be c(f, obs).
#'
#' @param def_content The def_content data.frame returned by read_def_content
#'
#' @return eqnvec. 

#' @export
#'
#' @examples
get_errors <- function(def_content) {
  errors <- eqnvec()
  
  if (any(def_content[["section"]] == "ERRORS")) {
    warning("It is assumed that only observables and error-pars occur in the equations of the errormodel")
    
    # fitErrorModel
    errors <- def_content[["def_content"]][def_content[["section"]]=="ERRORS"]
    errors <- str_split(errors, " ", simplify = T)
    errors <- data.frame(errors, stringsAsFactors = F)
    
    # Columns 1 are always well defined
    # Column 2 might be spread across more columns. Paste these expressions back together
    errors_eqns <- errors[2:length(errors)] %>% apply(1, paste , collapse = " ")
    errors <- data.frame(errors[1], errors_eqns, stringsAsFactors = F)
    
    errors_names <- c("name", "eqn")
    names(errors) <- errors_names[seq_along(errors)]
    
    errors <- structure(errors[["eqn"]], names = errors[["name"]])  %>% as.eqnvec() 
    
  }
  return(errors)
}



#' Read SUBSTITUTIONS
#' 
#' The substitutions are inserted iteratively. 
#' Since I don't know in which order, 
#' I botched a little by running resolveRecurrence(rev(substitutions)) a few times.
#'
#' @param def_content The def_content data.frame returned by read_def_content
#'
#' @return eqnvec containing the substitutions.
#' @export
#'
#' @examples
get_substitutions <- function(def_content) {
  substitutions <- eqnvec()
  # debugging
  # substitutions <- data.frame(name = "init_B_state", eqn = "penner")
  
  if (any(def_content[["section"]] == "SUBSTITUTIONS")) {
    
    # this also has to be done before STATES/ODEs to resolveRecurrence
    substitutions <- def_content[["def_content"]][def_content[["section"]]=="SUBSTITUTIONS"]
    substitutions <- str_split(substitutions, " ", simplify = T)
    substitutions <- data.frame(substitutions, stringsAsFactors = F)
    
    # Columns 1 are always well defined
    # Column 2 might be spread across more columns. Paste these expressions back together
    substitutions_eqns <- substitutions[2:length(substitutions)] %>% apply(1, paste , collapse = " ")
    # substitutions <- data.frame(substitutions[1], substitutions_eqns, stringsAsFactors = F)
    
    substitutions <- structure(substitutions_eqns, names = substitutions[[1]])
    
    # insert substitutions into each other
    # debugging
    # dput(eval(substitutions))
    # substitutions <- structure(c("k0 * k1", "k2", "init_A_state"), .Names = c("init_A_state",
    # "k1", "k3"))
    mysubstitutions <- NULL
    i<-1
    while((!identical(mysubstitutions, substitutions))&i<20) {
      mysubstitutions <<- rev(substitutions)
      substitutions <<- resolveRecurrence(rev(substitutions))
      i<<-i+1
    }
    
    substitutions <- substitutions %>% as.eqnvec()
  }
  
  return(substitutions)
}



#' Read CONDITIONS
#' 
#' The conditions section returns one condition in the dMod-sense. It basically returns a trafo with the parameters fixed/changed by this condition 
#'
#' @param def_content The def_content data.frame returned by read_def_content
#' @param derived derived get_derived(def_content) - data.frame with mandatory columns c("name", "eqn")
#' @param substitutions eqnvec containing the substitutions
#' @param states This has to be supplied from get_states(model_def)
#'
#' @return eqnvec with ONE CONDITION in the dMod-sense
#' @export
#'
#' @examples
get_conditions <- function(def_content, 
                           states = get_states(def_content),
                           derived = get_derived(def_content),
                           substitutions = get_substitutions(def_content)
                           ) {
  trafo <- eqnvec()

  if (any(def_content[["section"]] == "CONDITIONS")) {
    trafo <- def_content[["def_content"]][def_content[["section"]]=="CONDITIONS"]

    # replace derived variables by their definitions
    # don't know if this really was needed
    # potential pitfall: iterative substitutions
    if (!is.null(derived)) {
      myderived <- structure(paste("(", derived[["eqn"]], ")"), 
                             names = paste0('\\b', derived[["name"]], '\\b'))
      trafo <- trafo %>% str_replace_all(myderived)
    }  
    
    trafo <- str_split(trafo, " ", simplify = T)
    trafo <- data.frame(trafo, stringsAsFactors = F)
    
    # Columns 1 are always well defined
    # Column 2 might be spread across more columns. Paste these expressions back together
    trafo_eqns <- trafo[2:length(trafo)] %>% apply(1, paste , collapse = " ") 
    
    # in column 2, replace substituted variables by their definitions
    if (!is.null(substitutions)) {
      trafo_eqns <- replaceSymbols(what = names(substitutions),
                                   by = substitutions, 
                                   x = trafo_eqns)
    }
    
    
    trafo <- data.frame(trafo[1], trafo_eqns, stringsAsFactors = F)
    
    trafo_names <- c("name", "eqn")
    names(trafo) <- trafo_names[seq_along(trafo)]
    
    
    # # Seems to be not a problem:
    # # replace "init_" by "". First, check, if there is any conflict with "states"
    # mystates <- states[["name"]]
    # mycheck <-  sapply(paste0('[\b_]', states[["name"]], '[\b_]'), . %>% str_detect(unlist(trafo),.))
    # if (any(mycheck)) warning("The states themselves are in the trafo of the condition. Removing 'init_' might cause conflicts.")
    
    trafo <- lapply(trafo, . %>% str_replace_all("init_", "")) %>% as.data.frame(stringsAsFactors = F)
    trafo <- structure(trafo[["eqn"]], names = trafo[["name"]])
    
  }
  return(trafo)
}


#' Read RANDOM
#' 
#' The conditions section returns one condition in the dMod-sense. It basically returns a trafo with the parameters fixed/changed by this condition 
#'
#' @param def_content The def_content data.frame returned by read_def_content
#'
#' @return Character vector with parameter names which are condition specific
#' @export
#'
#' @examples
get_random <- function(def_content) {
  random_pars <- NULL
  
  if (any(def_content[["section"]] == "RANDOM")) {
    random_pars <- def_content[["def_content"]][def_content[["section"]]=="RANDOM"]
    
    random_pars <- str_split(random_pars, " ", simplify = T)
    random_pars <- data.frame(random_pars, stringsAsFactors = F)
    
    random_pars_names <- c("name", "type")
    names(random_pars) <- random_pars_names[seq_along(random_pars)]
    
    if (any(random_pars[["type"]] != "INDEPENDENT")) {
      warning("Some random parameters are not INDEPENDENT. Only INDEPENDENT parameters can be dealt with at the moment.")
    }
    
  }
  return(random_pars[["name"]])
}




#' Get fitted pars from Readme.txt
#'
#' If a Readme.txt file is available, it usually provides fitted parameters. Extract them.
#' 
#' This function is written rather dirtily
#' 
#' @param readme_file 
#'
#' @return
#' @export
#'
#' @examples
get_fitted_pars_from_readme <- function(readme_file) {
  readme <- readLines(readme_file)
  
  # Determine which line belongs to section "Parameters"
  section_headings <- readme[grep("Parameters", readme)]
  section_numbers <- cumsum(sapply(readme, . %>% str_detect(c("Original folder", "Model states", "Observables and Errors", "Parameters", "Please write")) %>% any) %>% unname)
  
  readme <- data.frame(readme = readme, section = c("Original folder", "Model states", "Observables and Errors", "Parameters", "Please write")[section_numbers], stringsAsFactors = F)
  
  pars_fitted <-
    readme %>% 
    dplyr::filter(section == "Parameters") %>% 
    dplyr::filter(str_detect(readme, "^#")) %>% 
    magrittr::extract2("readme") %>% 
    str_split("\\|", simplify = T) %>% 
    as.data.frame(stringsAsFactors = F) %>% 
    magrittr::set_names(c("number", "dyn_ini", "name","lb_log10value_ub","value", "fitted","prior"))  %>% {.}
    within(pars_fitted,{
      # split ub lb value ub
      mysplit <- str_split(lb_log10value_ub, " ", simplify = T) %>% apply(1, .%>% as.numeric() %>% magrittr::extract(!is.na(.)) %>% matrix(ncol = 3)) %>% t %>% magrittr::set_colnames(c("lb", "log10value", "ub"))
      lb <- mysplit[,1]
      ub <- mysplit[,3]
      log10value = mysplit[,2]
      remove(lb_log10value_ub, mysplit)
      
      # split up value
      mysplit <- str_split(value, " ", simplify = T)%>% apply(1, . %>% as.numeric() %>% magrittr::extract(!is.na(.)) %>% matrix(ncol = 2)) %>% t
      value <- mysplit[,2]
      to_pow_ten <- mysplit[,1]
      remove(mysplit)
    }) %>% 
      mutate(name = str_replace_all(name, "\\s", "")) %>% 
      mutate(name = str_replace_all(name, "init_", ""))
  
}








