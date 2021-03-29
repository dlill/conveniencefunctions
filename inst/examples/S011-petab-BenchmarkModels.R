# library(conveniencefunctions)
devtools::load_all("~/Promotion/Promotion/Software/dMod")
devtools::load_all("~/Promotion/Promotion/Projects/conveniencefunctions")
try(setwd(dirname(rstudioapi::getSourceEditorContext()$path)))

setwd("/home/daniel/Promotion/Promotion/Projects/conveniencefunctions/inst/examples")
warning("absolute file path")
# -------------------------------------------------------------------------#
# Fix getConditions ----
# -------------------------------------------------------------------------#
f <- petab_files("petab/BenchmarkModels/Fujita_SciSignal2010.petab", F,T)

conditions <- f$experimentalCondition
data       <- f$measurementData
pe_observables <- fread(f$observables)

# Create enzyme kinetics model and data ----
# -------------------------------------------------------------------------#
# debugonce(getReactionsSBML)
# debugonce(getConditionsSBML)
# pd <- importPEtabSBML_indiv("petab/enzymeKinetics.petab")
# ..  -----
# debugonce(getParametersSBML)
# debugonce(sensitivitiesSymb)
# ..  -----

# debugonce(importPEtabSBML_indiv)
# debugonce(getConditionsSBML)
# pd <- importPEtabSBML_indiv("petab/BenchmarkModels/Fujita_SciSignal2010.petab")

# ..  -----

# pd <- importPEtabSBML_indiv("petab/BenchmarkModels/Boehm_JProteomeRes2014.petab")
pd <- importPEtabSBML("Boehm_JProteomeRes2014", "petab/BenchmarkModels/")
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
