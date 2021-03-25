# -------------------------------------------------------------------------#
# Convenience functions ----
# -------------------------------------------------------------------------#

#' Original petab function didn't work
#'
#' @param model 
#' @param measurementData 
#'
#' @return
#' @export
#'
#' @examples
create_parameter_df <- function(model, measurementData) {
  speciesInfo <- model$speciesInfo
  parInfo <- model$parInfo
  par_sp <- petab_parameters(parameterId =   speciesInfo$speciesName, 
                             parameterName = speciesInfo$speciesName,
                             nominalValue =  speciesInfo$initialAmount,
                             estimate = as.numeric(speciesInfo$initialAmount > 0))
  par_pa <- petab_parameters(parameterId =   parInfo$parName, 
                             parameterName = parInfo$parName,
                             nominalValue =  parInfo$parValue)
  par_ob <- petab_parameters(parameterId =  getSymbols(measurementData$observableParameters), 
                             parameterName = getSymbols(measurementData$observableParameters),
                             parameterScale = "lin")
  par_meErr <- NULL
  if (length(getSymbols(measurementData$noiseParameters))) 
    par_meErr <- petab_parameters(parameterId =   getSymbols(measurementData$noiseParameters), 
                     parameterName = getSymbols(measurementData$noiseParameters),
                     nominalValue = 0.1)
  
  par <- rbindlist(list(par_sp, par_pa, par_ob, par_meErr))
}


#' dput for petab variables
#'
#' @param pe [petab()]
#' @param variable (single) character denoting a column, e.g. "observableId"
#'
#' @return dput output
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' 
petab_dput <- function(pe, variable) {
  dx <- copy(pe$measurementData)
  dx <- pe$experimentalCondition[dx, on = c("conditionId" = "simulationConditionId")]
  dx <- pe$observables[dx, on = c("observableId")]
  dput(unique(dx[[variable]]))
}


#' Title
#'
#' @param pe NULL: Default names, [petab()] Actual names in petab
#'
#' @return list of column names
#' @export
#'
#' @examples
petab_columns <- function(pe = NULL) {
  if(!is.null(pe))
      return(lapply(pe[c("experimentalCondition", 
                 "measurementData", 
                 "observables", 
                 "parameters")], names))
  
  m <- c("observableId","simulationConditionId","measurement","time","observableParameters","noiseParameters","datasetId","replicateId","preequilibrationConditionId")
  o <- c("observableId","observableName","observableFormula","observableTransformation","noiseFormula","noiseDistribution")
  e <- c("conditionId","conditionName")
  p <- c("parameterId","parameterName","parameterScale","lowerBound","upperBound","nominalValue","estimate","initializationPriorType","initializationPriorParameters","objectivePriorType","objectivePriorParameters")

  list(experimentalCondition = e, 
                measurementData = m, 
                observables = o, 
                parameters = p)
}

#' Create one big table containing measurementData, observables and experimentalCondition
#'
#' @param petab [petab]
#'
#' @return
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
petab_joinDCO <- function(pe) {
  if (length(pe$measurementData$preequilibrationConditionId) && 
      any(!is.na(pe$preequilibrationConditionId)))
    warning("DCO might not be able to handle preequilibrationConditionId")
  
  dx <- copy(pe$measurementData)
  dx <- pe$experimentalCondition[dx, on = c("conditionId" = "simulationConditionId")]
  dx <- pe$observables[dx, on = c("observableId")]
  dx
}

#' Unjoin DataConditonObs
#'
#' @param DCO A dco: a DataConditionObs data.table, from petab_joinDCO
#' @param pe [petab()] object
#'
#' @return pe with measurementData,experimentalCondition,Observables replaced by the columns created from DCO
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
petab_unjoinDCO <- function(DCO, pe = NULL) {
  # Get standard column names
  pc <- petab_columns(pe = pe)
  
  # Extract the tables
  # Excols: A bit overcautious with the names, when there is the potential to 
  #   pass a petab to pe it should be able to get the names 
  #   from that, but let's keep it for now.
  excols <- unique(c(intersect(names(DCO), pc$experimentalCondition), 
                     setdiff(names(DCO), c(pc$measurementData, pc$observables))))
  experimentalCondition = do.call(petab_experimentalCondition, DCO[,..excols])
  experimentalCondition <- unique(experimentalCondition)
  
  mecols <- intersect(names(DCO), c("conditionId", pc$measurementData))
  measurementData <- DCO[,..mecols]
  setnames(measurementData, "conditionId", "simulationConditionId") # need to name back
  measurementData = do.call(petab_measurementData, measurementData)
  
  obcols <- intersect(names(DCO), pc$observables)
  observables = do.call(petab_observables, DCO[,..obcols])
  observables <- unique(observables)
  
  # Do some sanity checks
  lostConditions <- setdiff(unique(pe$experimentalCondition$conditionId), 
                            unique(experimentalCondition$conditionId))
  if (length(lostConditions))
    warning("Conditions got lost in the transformation: ", paste0(lostConditions, collapse = ", "))
  
  # Return petab
  petab(
    model = pe$model,
    experimentalCondition = experimentalCondition,
    measurementData = measurementData,
    observables = observables,
    parameters = pe$parameters
    )

}


#' Consistently mutate the dco
#'
#' If j is supplied, the full dco is always returned and no subsetting is done!
#' 
#' @param pe [petab()]
#' @param i expression to subset the dco, can be missing
#' @param j Must be a call of the form `:=`(hsdhfgjs). If missing, pe is subsetted by i
#' 
#' @return pe with modified DCO
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
#' 
#' @examples 
#' # todo!!
petab_mutateDCO <- function(pe, i, j) {
  
  mi <- missing(i)
  mj <- missing(j)
  if ( mi &&  mj) return(pe)
  
  # Catch the arguments
  si <- substitute(i)
  sj <- substitute(j)
  
  # Probably unnecessary check that j is supplied properly
  pj <- getParseData(parse(text=deparse(sj)))
  is_set <- "`:=`" %in% pj[,"text"] # is the command a "set_*" command in the data.table sense?
  if (!is_set) stop("j should be called with `:=`")
  
  # Perform the data transformation on the DCO
  dco <- petab_joinDCO(pe)
  if (!mi &&  mj) {dco <- dco[eval(si)];   cat("subsetted dco\n")}       # filter
  if ( mi && !mj) {dco[,eval(sj)];         cat("mutated dco\n")}         # mutate
  if (!mi && !mj) {dco[eval(si),eval(sj)]; cat("mutated dco in rows\n")} # mutate_in_row
  
  # Return modified petab
  pe_out <- petab_unjoinDCO(dco, pe)
  pe_out
}

# -------------------------------------------------------------------------#
# Initializers ----
# -------------------------------------------------------------------------#

#' Constructor for Conditions
#'
#' https://petab.readthedocs.io/en/stable/documentation_data_format.html#condition-table
#'
#' @param conditionId character Condition ID
#' @param conditionName character Condition ID for plotting
#' @param ... parameterOrSpeciesOrCompartmentId1. Numeric (value) or string (different parameterId)
#' 
#' @return
#' 
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
petab_experimentalCondition <- function(
  conditionId,
  conditionName = NA,
  ...) {
  data.table(conditionId = conditionId, 
             conditionName = conditionName, 
             as.data.table(list(...)))
}


#' Constructor for Measurements 
#' 
#' @param observableId 
#' @param simulationConditionId 
#' @param measurement 
#' @param time 
#' @param observableParameters numeric string or NA
#' @param datasetId 
#' @param replicateId 
#' @param preequilibrationConditionId 
#' @param noiseParameters numeric, string or NA: Measurement noise or parameter name
#' 
#' 
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
petab_measurementData <- function(
  observableId,
  simulationConditionId,
  measurement,
  time,
  observableParameters        = list(NA, "1;1", "scale_obsi;offset_obsi")[[1]], 
  noiseParameters             = list(1, "error_ADD_obsi;error_REL_obsi")[[1]],
  datasetId                   = NA,
  replicateId                 = NA,
  preequilibrationConditionId = NA
) {
  data.table(
    observableId                = observableId,
    preequilibrationConditionId = preequilibrationConditionId,
    simulationConditionId       = simulationConditionId,
    measurement                 = measurement,
    time                        = time,
    observableParameters        = observableParameters,
    noiseParameters             = noiseParameters,
    datasetId                   = datasetId,
    replicateId                 = replicateId)
}

#' Constructor for Observables
#'
#' @param observableId 
#' @param observableName 
#' @param observableFormula 
#' @param observableTransformation 
#' @param noiseFormula 
#' @param noiseDistribution 
#'
#' @return
#' 
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
petab_observables <- function(
  observableId,
  observableName           = NA,
  observableFormula        = "observableParameter1_${observableId} * state1",
  observableTransformation = c("lin", "log", "log10")[[1]],
  noiseFormula             = c(1, "noiseParameter${n}_${observableId} + noiseParameter${n}_${observableId}*${observableId}")[[1]], # aka errormodel
  noiseDistribution        = c("normal", "laplace")[[1]]) {
  data.table(
    observableId             = observableId,
    observableName           = observableName,
    observableFormula        = observableFormula,
    observableTransformation = observableTransformation,
    noiseFormula             = noiseFormula,
    noiseDistribution        = noiseDistribution
  )
}

#' Constructor for Parameters
#'
#' @param parameterId 
#' @param parameterName 
#' @param parameterScale 
#' @param lowerBound 
#' @param upperBound 
#' @param nominalValue 
#' @param estimate 
#' @param initializationPriorType 
#' @param initializationPriorParameters 
#' @param objectivePriorType 
#' @param objectivePriorParameters 
#'
#' @return
#' 
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
petab_parameters <- function(
  parameterId,
  parameterName                 = NA,
  parameterScale                = c("log", "lin", "log10")[[1]],
  lowerBound                    = 0.0001, # given on linear scale
  upperBound                    = 1000,   # given on linear scale
  nominalValue                  = 1,      # given on linear scale
  estimate                      = c(1,0)[[1]],
  initializationPriorType       = c("parameterScaleUniform","uniform","normal","laplace","logNormal","logLaplace","parameterScaleNormal","parameterScaleLaplace")[[1]],
  initializationPriorParameters = "-1;1",
  objectivePriorType            = c("parameterScaleNormal","parameterScaleUniform","uniform","normal","laplace","logNormal","logLaplace","parameterScaleLaplace")[[1]],
  objectivePriorParameters      = "-1;1") {
  
  data.table(
    parameterId                   = parameterId,
    parameterName                 = parameterName,
    parameterScale                = parameterScale,
    lowerBound                    = lowerBound,
    upperBound                    = upperBound,
    nominalValue                  = nominalValue,
    estimate                      = estimate,
    initializationPriorType       = initializationPriorType,
    initializationPriorParameters = initializationPriorParameters,
    objectivePriorType            = objectivePriorType,
    objectivePriorParameters      = objectivePriorParameters
  )
}

#' PEtab structural model without sbml
#'
#' @param equationList eqnlist
#' @param events eventlist
#' @param ... not used, but could be used in the future for imitating assignment rules etc
#' 
#' @return list
#' 
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
petab_model <- function(equationList, events = NA, 
                        parInfo = getParInfo(equationList), 
                        speciesInfo = getSpeciesInfo(equationList),...) {
  list(equationList = equationList, events = events, 
       parInfo = parInfo, speciesInfo = speciesInfo, ...)
}


# -------------------------------------------------------------------------#
# PEtab representation ----
# -------------------------------------------------------------------------#

#' Collector function for petab files
#'
#' @param model [dMod::eqnlist()]
#' @param condition see [petab_condition()]
#' @param measurements see [petab_measurements()]
#' @param observables see [petab_observables()]
#' @param parameters see [petab_parameters()]
#'
#' @return list of the input arguments
#' 
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
petab <- function(
  model = NULL,
  experimentalCondition = NULL,
  measurementData = NULL,
  observables = NULL,
  parameters = NULL, 
  ...
) {
  
  # Think of valid combinations of NAs
  # either full specification without parameters
  # or sparse specification without parameters
  # or sparse specification with parameters
  
  petab <- list(model = model,
       experimentalCondition = experimentalCondition,
       measurementData = measurementData,
       observables = observables,
       parameters = parameters)
  
  petab_lint(petab)
  
  petab
}

#' Title
#'
#' @param filename path ending in .petab
#'
#' @return list(modelname, path)
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
#'
#' @examples
petab_modelname_path <- function(filename) {
  if (tools::file_ext(basename(filename)) != "petab") 
    stop("File ending should be .petab (is ", tools::file_ext(filename), ")")
  modelname <- gsub(".petab$", "", basename(filename))
  path <- gsub(".petab$", "", filename)
  list(modelname = modelname, path = path)
}


#' List petab files
#'
#' @param FLAGTestCase generate TestCases filename
#' @param filename "path/to/modelname.petab". Will generate filenames like 
#'        "path/to/modelname/model_modelname.xml"
#' @param FLAGreturnList return list or vector?
#' 
#' @return list or character vector of file paths
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
#' 
#' @examples
#' petab_files("Models/Example.petab") 
petab_files <- function(filename, FLAGTestCase = FALSE, FLAGreturnList = FALSE) {
  
  modelname <- petab_modelname_path(filename)$modelname
  path <- petab_modelname_path(filename)$path
  
  # [ ] warning("model refers to rds instead of xml\n")
  out <- NULL
  if (FLAGTestCase) {
    out <- c(
      yaml                       = paste0(modelname, ".yaml"),
      experimentalCondition      = paste0("_experimentalCondition"     , ".tsv"),
      measurementData            = paste0("_measurementData"           , ".tsv"),
      modelXML                      = paste0("_model"                     , ".xml"), 
      # [ ] not very elegant. Remove rds when sbml is stable
      model                      = paste0("_model"                     , ".rds"),
      observables                = paste0("_observables"               , ".tsv"),
      parameters                 = paste0("_parameters"                , ".tsv"),
      simulatedData              = paste0("_simulatedData"             , ".tsv"),
      visualizationSpecification = paste0("_visualizationSpecification", ".tsv"))
  } else {
    out <- c(
      yaml                       = paste0(modelname, ".yaml"),
      experimentalCondition      = paste0("experimentalCondition_"     , modelname, ".tsv"),
      measurementData            = paste0("measurementData_"           , modelname, ".tsv"),
      modelXML                      = paste0("model_"                     , modelname, ".xml"),
      # [ ] not very elegant. Remove rds when sbml is stable
      model                      = paste0("model_"                     , modelname, ".rds"),
      observables                = paste0("observables_"               , modelname, ".tsv"),
      parameters                 = paste0("parameters_"                , modelname, ".tsv"),
      simulatedData              = paste0("simulatedData_"             , modelname, ".tsv"),
      visualizationSpecification = paste0("visualizationSpecification_", modelname, ".tsv"))
  }
  nm <- names(out)
  out <- setNames(file.path(path, out), nm)
  if (FLAGreturnList) out <- as.list(out)
  out
}


#' Read PEtab files
#'
#' @param modelname 
#' @param path 
#' @param FLAGTestCase 
#'
#' @return
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
#'
#' @examples
readPetab <- function(filename, FLAGTestCase = FALSE) {
  
  files <- petab_files(filename = filename, FLAGTestCase = FLAGTestCase)
  files <- files[file.exists(files)]
  # tables
  files_tsv <- grep("tsv", files, value = TRUE)
  files_tsv <- lapply(files_tsv, fread)
  # model
  files_model <- grep("xml", files, value = TRUE) # Do nothing, read rds

  files_model <- grep("rds", files, value = TRUE)
  files_model <- lapply(files_model, readRDS)
  
  do.call(petab, c(files_model, files_tsv))
}

#' Title
#'
#' @param petab 
#' @param filename 
#'
#' @return
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
#'
#' @examples
writePetab <- function(petab, filename = "petab/model.petab") {

  # Create folder, load petab
  dir.create(petab_modelname_path(filename)$path, FALSE, TRUE)
  pe <- petab_python_setup()
  
  # Get filenames
  files <- petab_files(filename = filename)
  
  # Write yaml
  pe$create_problem_yaml(sbml_files        = basename(files["modelXML"]),
                         condition_files   = basename(files["experimentalCondition"]),
                         measurement_files = basename(files["measurementData"]),
                         parameter_file    = basename(files["parameters"]), 
                         observable_files  = basename(files["observables"]), 
                         yaml_file         = files["yaml"])
  
  # [ ] Hack: Remove once sbml export is stable
  if ("model" %in% names(petab)) petab$modelXML <- petab$model
  
  # Select files to write
  files <- files[names(petab)]
  files <- files[vapply(petab, function(x) !is.null(x), TRUE)]
  
  # Write tables
  files_tsv <- grep("tsv", files, value = TRUE)
  if (length(files_tsv)) 
    lapply(names(files_tsv), function(nm) {
      fwrite(petab[[nm]], files[[nm]], sep = "\t")})
  
  # Write model rds
  files_model <- grep("rds", files, value = TRUE)
  if (length(files_model)) 
    lapply(names(files_model), function(nm) {
      saveRDS(petab[[nm]], files[[nm]])})
  
  # Write model xml
  files_model <- grep("xml", files, value = TRUE)
  if (length(files_model)) {
    args <- c(petab$model, list(filename = files_model, 
                                modelname = modelname))
    args <- args[setdiff(names(args), "events")] # [ ] Todo: Events
    do.call(sbml_exportEquationList, args)
  }
  
  cat("Success?")
  invisible(petab)
}


# -------------------------------------------------------------------------#
# Interface to useful PEtab.py functions ----
# -------------------------------------------------------------------------#

#' Title
#'
#' @param petab 
#'
#' @return list of errors
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
#'
#' @examples
petab_lint <- function(petab) {
  
  # [ ] Implement access to petab.lint
  errlist <- list()
  
  # Some quick own checks
  dupes <- which(duplicated(petab$measurementData))
  if(length(dupes)) {
    warning("These rows are duplicates in measurementData: ", paste0(head(dupes,10), collapse = ","), "...")
    errlist <- c(errlist, measurementDataDupes = dupes)}
  
  dupes <- which(duplicated(petab$observables$observableID))
  if(length(dupes)) {
    warning("These rows are duplicates in observableId: ", paste0(head(dupes,10), collapse = ","), "...")
    errlist <- c(errlist, observableIdDupes = dupes)}

  dupes <- which(duplicated(petab$experimentalCondition$conditionId))
  if(length(dupes)) {
    warning("These rows are duplicates in conditionId :", paste0(head(dupes,10), collapse = ","), "...")
    errlist <- c(errlist, list(conditionIdDupes = dupes))}
  
  errlist
}



# -------------------------------------------------------------------------#
# Python setup ----
# -------------------------------------------------------------------------#

#' Setup the connection to python petab
#' 
#' * if petab virtualenv not present: 
#'   * sets up a virtualenv called "petab" 
#'   * pip installs petab
#' * uses "petab" virtual env
#' * imports and returns petab
#' 
#' use as pe <- petab_python_setup()
#' 
#' @return python module, see [reticulate::import()]
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
#' 
#' @examples 
#' pepy <- petab_python_setup
#' pepy$lint
petab_python_setup <- function() {
  if (!"petab" %in% reticulate::virtualenv_list()){
    reticulate::virtualenv_install("petab", "petab")
  }
  message("Using petab virtualenv\n")
  reticulate::use_virtualenv("petab")
  reticulate::import("petab")
}


# -------------------------------------------------------------------------#
# PEtab import ----
# -------------------------------------------------------------------------#
#' Parse the parameters columns in measurementData
#'
#'
#' @param measurementData petab$measurementData. Uses only columns observableId,simulationConditionId and "column"
#' @param column character(1L). one of c("observableParameters", "noiseParameters")
#'
#' @return data.table(condition, Innerpars... = Outerpars...) of local parameters.
#'   Can be added to a gridlist with [dMod::indiv_addLocalParsToGridlist()]
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
#' 
#' @examples 
#' measurementData <- data.table(
#' observableId = c("obsE"),
#' simulationConditionId = c("C1", "C2"),
#' observableParameters = c("scale;offset", "1;offset"),
#' noiseParameters = "sigmaAbs"
#' )
#' 
#' petab_getMeasurementParsMapping(measurementData, "noiseParameters")
#' petab_getMeasurementParsMapping(measurementData, "observableParameters")
#' # suggested use: indiv_addLocalParsToGridlist ... 
petab_getMeasurementParsMapping <- function(measurementData, column = c("observableParameters", "noiseParameters")[1]) {
  
  # Select column and name
  parameters <- measurementData[[column]]
  parameterString <- gsub("Parameters", "Parameter", column)
  
  # Pipeline of death
  mp <- strsplit(parameters, ";")
  mp <- lapply(mp, function(x) {if(length(x)) return(as.data.table(as.list(x))) else data.table(NA)})
  mp <- rbindlist(mp, fill = TRUE)
  setnames(mp, paste0(parameterString, 1:length(mp), "_"))
  mp <- data.table(observableId = measurementData$observableId, condition = measurementData$simulationConditionId, mp)
  mp <- melt(mp, id.vars = c("observableId", "condition"), variable.name = "INNERPARAMETER", variable.factor = FALSE, value.name = "OUTERPARAMETER")
  mp <- unique(mp)
  mp <- mp[!is.na(OUTERPARAMETER)]
  mp[,`:=`(INNERPARAMETER = paste0(INNERPARAMETER, observableId))]
  mp <- mp[,list(condition, INNERPARAMETER, OUTERPARAMETER)]
  mp <- dcast(mp, condition ~ INNERPARAMETER, value.var = "OUTERPARAMETER")
  mp
}


petab_getMeasurementParsScales <- function(measurementData,parameters) {
  
  # >>>>>>> obsolete function!
  # >>>>>>> functionality covered currently by 
  # >>>>>>> dMod::updateParscalesToBaseTrafo <<<<<<<<
  
  parameters <- pe$parameters
  measurementData <- pe$measurementData
  
  # measurementPars
  mp <- lapply(c("observableParameters", "noiseParameters"), function(column) {
    petab_getMeasurementParsMapping(measurementData, column)})
  mp <- merge(mp[[1]],mp[[2]])
  mp <- melt(mp, id.vars = "condition", 
             variable.name = "INNERPARAMETER", 
             variable.factor = FALSE, value.name = "parameterId")
  
  # parScales
  ps <- parameters[mp, on = c("parameterId") ]
  ps <- ps[,list(INNERPARAMETER, parameterScale)]
  ps <- unique(ps)
  
  # check that trafo is the same in all conditions
  dupeScale <- duplicated(ps$INNERPARAMETER)
  if (any(dupeScale)) 
    stop("Model parameter has two different scales mapped to it in different conditions: ", 
         ps$INNERPARAMETER[dupeScale])
  
  list(parScales = setNames(ps$parameterScale, ps$INNERPARAMETER), 
       coveredPars = unique(mp$parameterId))
}


# -------------------------------------------------------------------------#
# Plotting ----
# -------------------------------------------------------------------------#

#' Plot Petab data
#'
#' Automatic multipage export in the background with the future package and ggforce::facet_wrap_paginate
#'
#' @param petab A [petab] object
#' @param conditionId,observableId,datasetId Subset data to plot. Find out suitable values with [petab_dput()]
#' @param aeslist list of formulas used to call [ggplot2::aes_q()]. 
#'  Default aesthetics are list(x = ~time, y = ~measurement, color = ~conditionId). 
#'  Can be overriden by aeslist
#' @param ggCallback additional stuff to add to the ggplot, such as a call to [ggplots2::labs()] or scales
#' @param FLAGmeanLine draw line connecting the means
#' @param paginateInfo see [conveniencefunctions::cf_paginateInfo()]
#' @param FLAGfuture export asynchronously with the future package
#' @param filename,width,height,scale,units see [ggplot2::ggsave()]
#'
#' @return ggplot
#' 
#' @importFrom future plan "%<-%"
#' 
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
petab_plotData <- function(petab, 
                           conditionId = NULL, 
                           observableId = NULL, 
                           datasetId = NULL,
                           aeslist = NULL,
                           FLAGUseObservableTransformation = TRUE,
                           FLAGmeanLine = TRUE,
                           paginateInfo=cf_paginateInfo(facets = ~observableId,nrow = 4,ncol = 4,scales = "free", type = "wrap"),
                           
                           ggCallback = geom_blank(),
                           
                           filename = NULL, 
                           FLAGfuture = TRUE,
                           width = 21, height = 29.7, scale = 1, units = "cm"
) {
  
  # create plotting data.table
  dplot <- petab_joinDCO(petab)
  if (length(conditionId)) {rowsub <- dplot[,conditionId %in% ..conditionId]; dplot <- dplot[rowsub]}
  if (length(observableId)) {rowsub <- dplot[,observableId %in% ..observableId]; dplot <- dplot[rowsub]}
  # apply log transformation to data when applicable
  if (FLAGUseObservableTransformation) 
    dplot[observableTransformation != "lin",
          `:=`(measurement = eval(parse(text = paste0(observableTransformation, "(measurement)"))),
               observableId = paste0(observableTransformation, " ", observableId))]
  
  # mean values to draw lines
  if (FLAGmeanLine){
    byvars <- lapply(aeslist, function(x) cOde::getSymbols(as.character(x)))
    byvars <- do.call(c, byvars)
    byvars <- unique(c("observableId", "time", "conditionId", byvars))
    dmean <- copy(dplot)
    dmean <- dmean[,list(measurement = mean(measurement)), by = byvars]
  }
  
  # handle aesthetics
  aes0 <- list(x = ~time, y = ~measurement, color = ~conditionId)
  aeslist <- c(aeslist, aes0[setdiff(names(aes0), names(aeslist))])
  
  # handle facets
  facet_paginate <- utils::getFromNamespace(paginateInfo$type, "ggforce")
  paginateInfo0 <- paginateInfo[setdiff(names(paginateInfo), "type")]
  
  
  # Create plot
  pl <- cfggplot() 
  pl <- pl + {do.call(facet_paginate, paginateInfo0)}
  
  if (FLAGmeanLine) {
    aesmean0 <- list(linetype = ~conditionId, group = ~conditionId)
    aesmeanlist <- c(aeslist, aesmean0[setdiff(names(aesmean0), names(aeslist))])
    pl <- pl + geom_line(do.call(aes_q, aesmeanlist), data = dmean)
  }
  pl <- pl + geom_point(do.call(aes_q, aeslist), data = dplot)
  pl <- pl + ggCallback
  
  # Print paginate message so user doesnt forget about additional pages
  message("Plot has ", ggforce::n_pages(pl), " pages\n")
  
  # output
  if (!is.null(filename)){
    if(FLAGfuture) {
      if (!"multisession" %in% class(future::plan())) future::plan("multisession")
      # assign(".Last", function() plan("sequential"), .GlobalEnv)
    }
    future::`%<-%`(wup,cf_outputFigure(pl = pl, filename = filename,
                             width = width, height = height, scale = scale, units = units, 
                             paginateInfo = paginateInfo))
    assign(paste0(".dummy", round(runif(1),4)), wup, .GlobalEnv) # so that the reference is not deleted and future evaluates until the end
    
    return(invisible(pl))
  }
  
  pl
}

# -------------------------------------------------------------------------#
# Overview tables ----
# -------------------------------------------------------------------------#

#' Generate overview table which observables are in which condition
#'
#' @param pe [petab()] object
#' @param Ntruncate truncate pasted observables at this many characters
#' @param ... 
#'
#' @return
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
petab_overviewObsPerCond <- function(pe, Ntruncate = Inf, ...) {
  dx <- petab_joinDCO(pe)
  if ("conditionName" %in% names(dx)) dx[,`:=`(conditionName = conditionId)]
  dx <- dx[,list(observableId = paste0(sort(unique(observableId)), collapse = ",")), 
           by = c("conditionId", "conditionName")]
  dx <- dx[,`:=`(observableId = substr(observableId, 1, Ntruncate))]
  cfoutput_MdTable(dx, ...)
}


# -------------------------------------------------------------------------#
# Todolist ----
# -------------------------------------------------------------------------#
# * Sample from prior, ...
# * petablint ...
# Next steps
# [ ] Construct measurement data from old data
# [ ] Construct experimentalCondition for model
# [ ] Construct observables: Think about specific functions to generate the observables "on the fly" for different model specifications
# [ ] Construct model
# [ ] Construct parameters: From rest of petab file

