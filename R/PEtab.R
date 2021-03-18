# -------------------------------------------------------------------------#
# Tables ----
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

# -------------------------------------------------------------------------#
# Model ----
# -------------------------------------------------------------------------#

#' PEtab structural model without sbml
#'
#' @param odes eqnlist or eqnvc
#' @param events eventlist
#' @param ... not used, but could be used in the future for imitating assignment rules etc
#' 
#' @return list
#' 
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
petab_model <- function(odes, events = NA, ...) {
  list(odes = odes, events = events, ...)
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
  path <- dirname(filename)
  list(modelname = modelname, path = path)
}


#' List petab files
#' @param modelname 
#' @param path 
#' @param FLAGTestCase 
#' modelname <- "Boehm_JProteomeRes2014"
#' path <- "../../Software/dMod/BenchmarkModels/Boehm_JProteomeRes2014/"
#' FLAGTestCase <- FALSE
#' path <- "../../Software/dMod/PEtabTests/0001"
#' FLAGTestCase <- TRUE
petab_files <- function(filename, FLAGTestCase = FALSE) {
  
  modelname <- petab_modelname_path(filename)$modelname
  path <- petab_modelname_path(filename)$path
  
  # [ ] warning("model refers to rds instead of xml\n")
  out <- NULL
  if (FLAGTestCase) {
    out <- c(
      yaml                       = paste0(modelname, ".yaml"),
      experimentalCondition      = paste0("_experimentalCondition"     , ".tsv"),
      measurementData            = paste0("_measurementData"           , ".tsv"),
      modelXML                   = paste0("_model"                     , ".xml"),
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
      modelXML                   = paste0("model_"                     , modelname, ".xml"),
      model                      = paste0("model_"                     , modelname, ".rds"),
      observables                = paste0("observables_"               , modelname, ".tsv"),
      parameters                 = paste0("parameters_"                , modelname, ".tsv"),
      simulatedData              = paste0("simulatedData_"             , modelname, ".tsv"),
      visualizationSpecification = paste0("visualizationSpecification_", modelname, ".tsv"))
  }
  nm <- names(out)
  setNames(file.path(path, out), nm)
}


#' Read PEtab files
#'
#' @param modelname 
#' @param path 
#' @param FLAGTestCase 
#'
#' @return
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
  files_model <- grep("xml", files, value = TRUE)
  if (length(files_model)) stop("xml not supported")
  files_model <- grep("rds", files, value = TRUE)
  files_model <- lapply(files_model, readRDS)
  
  do.call(petab, c(files_model, files_tsv))
}

writePetab <- function(petab, filename = "petab/model.petab", FLAGTestCase = FALSE) {

  # Create folder
  dir.create(petab_modelname_path(filename)$path, FALSE, TRUE)
  
  # Select files to write
  files <- petab_files(filename = filename, FLAGTestCase = FLAGTestCase)
  files <- files[names(petab)]
  files <- files[vapply(petab, function(x) !is.null(x), TRUE)]
  
  # Write tables
  files_tsv <- grep("tsv", files, value = TRUE)
  if (length(files_tsv)) 
    lapply(names(files_tsv), function(nm) {
      fwrite(petab[[nm]], files[[nm]], sep = "\t")})
  
  # Write model
  files_model <- grep("rds", files, value = TRUE)
  if (length(files_model)) 
    lapply(names(files_model), function(nm) {
      saveRDS(petab[[nm]], files[[nm]])})
  
  # [ ] Write xml model 
  
  # [ ] Write yaml
  
  invisible(petab)
}


# -------------------------------------------------------------------------#
# Interface to useful PEtab functions ----
# -------------------------------------------------------------------------#

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


# * Sample from prior, ...
# * petablint ...


# Next steps
# [ ] Construct measurement data from Viktor's data
# [ ] Construct experimentalCondition for model
# [ ] Construct observables: Think about specific functions to generate the observables "on the fly" for different model specifications
# [ ] Construct model
# [ ] Construct parameters: From rest of petab file

# [ ] 
