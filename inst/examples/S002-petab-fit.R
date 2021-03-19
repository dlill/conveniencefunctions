# library(conveniencefunctions)
devtools::load_all("~/Promotion/Promotion/Software/dMod")
devtools::load_all("~/Promotion/Promotion/Projects/conveniencefunctions")
try(setwd(dirname(rstudioapi::getSourceEditorContext()$path)))

# -------------------------------------------------------------------------#
# Create enzyme kinetics model and data ----
# -------------------------------------------------------------------------#
# pd <- importPEtabSBML_indiv("petab/enzymeKinetics.petab")
# debugonce(importPEtabSBML_indiv)
pd <- importPEtabSBML_indiv("./enzymeKinetics.petab")

# cfoutput_MdTable(getSpeciesInfo(el), NFLAGtribble = 2)
# sbml_exportEquationList(el, filename, parInfo = parInfo)
