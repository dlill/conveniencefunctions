# library(conveniencefunctions)
devtools::load_all("~/Promotion/Promotion/Software/dMod")
devtools::load_all("~/Promotion/Promotion/Projects/conveniencefunctions")
try(setwd(dirname(rstudioapi::getSourceEditorContext()$path)))

setwd("/home/daniel/Promotion/Promotion/Projects/conveniencefunctions/inst/examples")
warning("absolute file path")
# -------------------------------------------------------------------------#
# Create enzyme kinetics model and data ----
# -------------------------------------------------------------------------#
# debugonce(importPEtabSBML_indiv)
# debugonce(getReactionsSBML)
pd <- importPEtabSBML_indiv("petab/enzymeKinetics.petab")

# cfoutput_MdTable(getSpeciesInfo(el), NFLAGtribble = 2)
# sbml_exportEquationList(el, filename, parInfo = parInfo)

# Exit ----