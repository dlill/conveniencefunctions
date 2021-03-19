# library(conveniencefunctions)
devtools::load_all("~/Promotion/Promotion/Software/dMod")
devtools::load_all("~/Promotion/Promotion/Projects/conveniencefunctions")
try(setwd(dirname(rstudioapi::getSourceEditorContext()$path)))

setwd("/home/daniel/Promotion/Promotion/Projects/conveniencefunctions/inst/examples")
warning("absolute file path")
# -------------------------------------------------------------------------#
# Create enzyme kinetics model and data ----
# -------------------------------------------------------------------------#
# debugonce(getReactionsSBML)
# debugonce(getParametersSBML)
# debugonce(sensitivitiesSymb)

# debugonce(importPEtabSBML_indiv)
pd <- importPEtabSBML_indiv("petab/enzymeKinetics.petab")
# debugonce(importPEtabSBML)
# pd <- importPEtabSBML("enzymeKinetics","petab/")

pd$prd(seq(0,100), pd$pars)

# cfoutput_MdTable(getSpeciesInfo(el), NFLAGtribble = 2)
# sbml_exportEquationList(el, filename, parInfo = parInfo)

# Exit ----