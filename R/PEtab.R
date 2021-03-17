

#' Title
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
  data.table(conditionId = conditionId, conditionName = conditionName, as.data.table(list(...)))
}


#' @param observableParameters numeric string or null
#' @param noiseParameters numeric, string or null: Measurement noise or parameter name
petab_measurements <- function(
observableId,
preequilibrationConditionId = NULL,
simulationConditionId,
measurement,
time,
observableParameters = list(NULL, "1;1", "scale_i;offset_i")[[1]], 
noiseParameters = list(1, "error_obsi_ADD;error_obsi_REL")[[1]],
datasetId = NULL,
replicateId = NULL) {
data.table(
  observableId = observableId,
  preequilibrationConditionId = preequilibrationConditionId,
  simulationConditionId = simulationConditionId,
  measurement = measurement,
  time = time,
  observableParameters = observableParameters,
  noiseParameters = noiseParameters,
  datasetId = datasetId,
  replicateId = replicateId)
}

# For noiseDistribution, normal and laplace are supported. For observableTransformation, lin, log and log10 are supported. Denote by 𝑦 the simulation, 𝑚 the measurement, and 𝜎 the standard deviation of a normal, or the scale parameter of a laplace model, as given via the noiseFormula field. Then we have the following effective noise distributions.
petab_observables <- function(
  observableId,
  observableName = NULL,
 	observableFormula = "observableParameter${n}_${observableId} * state1",
  observableTransformation = c("lin", "log", "log10"),
 	noiseFormula = c(1, "noiseParameter${n}_${observableId}"), # aka errormodel
  noiseDistribution = c("normal", "laplace")) {

}

# petab_parameter
petab_parameter <- function(
parameterId,
parameterName = NULL,
parameterScale = c("log", "lin", "log10"),
lowerBound = 0,     # given on linear scale
upperBound = 10000, # given on linear scale
nominalValue = 1,   # given on linear scale
estimate = c(0,1),
initializationPriorType = c("parameterScaleUniform","uniform","normal","laplace","logNormal","logLaplace","parameterScaleNormal","parameterScaleLaplace"),
initializationPriorParameters = "-1;1",
objectivePriorType = c("parameterScaleNormal","parameterScaleUniform","uniform","normal","laplace","logNormal","logLaplace","parameterScaleLaplace"),
objectivePriorParameters = "-1;1") {

}



