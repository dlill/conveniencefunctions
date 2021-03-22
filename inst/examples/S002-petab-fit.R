# library(conveniencefunctions)
devtools::load_all("~/Promotion/Promotion/Software/dMod")
devtools::load_all("~/Promotion/Promotion/Projects/conveniencefunctions")
try(setwd(dirname(rstudioapi::getSourceEditorContext()$path)))

setwd("/home/daniel/Promotion/Promotion/Projects/conveniencefunctions/inst/examples")
warning("absolute file path")
# -------------------------------------------------------------------------#
# Create enzyme kinetics model and data ----
# -------------------------------------------------------------------------#
pe <- readPetab("petab/enzymeKinetics.petab")
# ..  -----
me <- copy(pe$measurementData)
simulationConditionId <- me$simulationConditionId
observableId          <- me$observableId
noiseParameters       <- me$noiseParameters




# getNoiseParMapping <- function(measurementData) {
#   
# }


# debugonce(getReactionsSBML)
# pd <- importPEtabSBML_indiv("petab/enzymeKinetics.petab")
# ..  -----
# debugonce(getParametersSBML)
# debugonce(sensitivitiesSymb)
# ..  -----
# debugonce(importPEtabSBML_indiv)
debugonce(petab_getMeasurementParsMapping)
pd <- importPEtabSBML_indiv("petab/enzymeKinetics.petab")
# ..  -----
# debugonce(importPEtabSBML)
# pd <- importPEtabSBML("enzymeKinetics","petab/")

# ..  -----
pd$prd(seq(0,100), pd$pars)
pd$obj_data(pd$pars)

myfit <- trust(pd$obj_data, pd$pars,1,10,iterlim = 1000)
plotCombined(pd$prd(seq(0,100), pd$pars), pd$data)
plotCombined(pd$prd(seq(0,100), myfit$argument), pd$data)

# cfoutput_MdTable(getSpeciesInfo(el), NFLAGtribble = 2)
# sbml_exportEquationList(el, filename, parInfo = parInfo)

# Exit ----
