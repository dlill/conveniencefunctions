

#' Constructor for Conditions
#'
#' https://petab.readthedocs.io/en/stable/documentation_data_format.html#condition-table
#'
#' @param conditionId character Condition ID
#' @param conditionName character Condition ID for plotting
#' @param ... parameterOrSpeciesOrCompartmentId1. Numeric (value) or string (different parameterId)
#' 
#' @return
#' @export
#'
#' @examples
petab_condition <- function(
  conditionId,
  conditionName = NULL,
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
#' @param observableParameters numeric string or null
#' @param datasetId 
#' @param replicateId 
#' @param preequilibrationConditionId 
#' @param noiseParameters numeric, string or null: Measurement noise or parameter name
petab_measurements <- function(
  observableId,
  simulationConditionId,
  measurement,
  time,
  observableParameters        = list(NULL, "1;1", "scale_i;offset_i")[[1]], 
  noiseParameters             = list(1, "error_obsi_ADD;error_obsi_REL")[[1]],
  datasetId                   = NULL,
  replicateId                 = NULL,
  preequilibrationConditionId = NULL
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
#' @export
#'
#' @examples
petab_observables <- function(
  observableId,
  observableName           = NULL,
  observableFormula        = "observableParameter${n}_${observableId} * state1",
  observableTransformation = c("lin", "log", "log10")[[1]],
  noiseFormula             = c(1, "noiseParameter${n}_${observableId}")[[1]], # aka errormodel
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
#' @export
#'
#' @examples
petab_parameters <- function(
  parameterId,
  parameterName                 = NULL,
  parameterScale                = c("log", "lin", "log10")[[1]],
  lowerBound                    = 0,     # given on linear scale
  upperBound                    = 10000, # given on linear scale
  nominalValue                  = 1,   # given on linear scale
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



