# library(conveniencefunctions)
devtools::load_all("~/Promotion/Promotion/Software/dMod")
devtools::load_all("~/Promotion/Promotion/Projects/conveniencefunctions")
try(setwd(dirname(rstudioapi::getSourceEditorContext()$path)))

# -------------------------------------------------------------------------#
# Create enzyme kinetics model and data ----
# -------------------------------------------------------------------------#
# .. Eqnlist and objects -----
modelname <- "enzymeKinetics"

el <- NULL
el <- addReaction(el, from = "E + S", to = "ES", rate = "(kon)*E*S",
                  description = "production of complex")
el <- addReaction(el, from = "ES", to = "E + S", rate = "koff*ES",
                  description = "decay of complex")
el <- addReaction(el, from = "ES", to = "E + P", rate = "kcat*ES",
                  description = "production of product")
el <- eqnlist_addDefaultCompartment(el, "cytoplasm") # Need compartment information for SBML

parInfo <- data.table(tibble::tribble(
  ~parName, ~parValue, ~parUnit,
  "kon"   ,     1  ,"litre_per_mole_per_second" ,    # Because of compartment, all dMod-fluxes are multiplied with cytoplasm volume
  "koff"  ,     0.1,"per_second" , 
  "kcat"  ,     0.1,"per_second" ))

speciesInfo <- data.table(tibble::tribble(
  ~speciesName, ~compName, ~initialAmount,
  "E"         ,"cytoplasm" ,             1,          # Amount, not concentration
  "S"         ,"cytoplasm" ,             100,
  "ES"        ,"cytoplasm" ,             1e-12,
  "P"         ,"cytoplasm" ,             1e-12))

# compartmentInfo is left as the default getCompartmentInfo(el)
# unitInfo is left as the default getUnitInfo(): If you need other units, you need to add them

# .. Simulate Data -----
compiled <- odemodel(f = el,modelname = modelname)
x <- Xs(compiled, condition = "C1")
pars <- c(setNames(parInfo$parValue, parInfo$parName),
          setNames(speciesInfo$initialAmount, speciesInfo$speciesName))

pred <- as.data.table(x(seq(0,100), pars)[[1]])
pred <- x(seq(0,100, 10), pars)
pred <- data.table(as.data.frame(pred))
pred <- pred[time > 0]
pred[,`:=`(sigma = 0.1)]
pred <- rbind(pred,pred,pred)
pred[,`:=`(value = exp(log(value) + rnorm(length(value), sd = sigma)))]
pred[,`:=`(name = paste0("obs", name))]

# -------------------------------------------------------------------------#
# Export Petab ----
# -------------------------------------------------------------------------#
# .. Create petab tables -----
pe_ex <- petab_experimentalCondition("C1", "C1")
pe_ob <- petab_observables(observableId = c("obsE","obsS","obsES","obsP"),
                           observableName = c("obsE","obsS","obsES","obsP"),
                           observableFormula = c("E","S","ES","P"), 
                           observableTransformation = "log",
                           noiseFormula = c("0.1"),
                           noiseDistribution = c("normal"))
pe_me <- petab_measurementData(observableId = pred$name,
                               simulationConditionId = "C1",
                               measurement = pred$value,
                               time = pred$time,
                               observableParameters = NA_character_,
                               noiseParameters = pred$sigma,
                               datasetId = "data1",
                               replicateId = rep(1:3, each = nrow(pred)/3),
                               preequilibrationConditionId = NA_character_)
# .. error model -----
pe_ob[,`:=`(noiseFormula = paste0("noiseParameter1_", observableId))]
pe_me[,`:=`(noiseParameters = paste0("sigma_", observableId))]

# ..  -----
pe_me[observableId == "obsE",`:=`(observableParameters = "offset_E")]
pe_mo <- petab_model(el,events = NULL,parInfo = parInfo, speciesInfo = speciesInfo)
pe_pa <- petab_create_parameter_df(pe_mo, pe_me)


# .. Create petab -----
pe <- petab(model = pe_mo,
                    experimentalCondition = pe_ex,
                    measurementData = pe_me,
                    observables = pe_ob,
                    parameters = pe_pa)

filename <- "petab/enzymeKinetics.petab"
writePetab(pe, filename)

# cfoutput_MdTable(getSpeciesInfo(el), NFLAGtribble = 2)
# sbml_exportEquationList(el, filename, parInfo = parInfo)


# -------------------------------------------------------------------------#
# Create Parameter_df ----
# -------------------------------------------------------------------------#
# 
# library(reticulate)
# files <- petab_files(filename, FLAGreturnList = TRUE)
# 
# 
# pepy <- petab_python_setup()
# sbmlmodel <- pepy$get_sbml_model(files$modelXML)
# pepy$petab_create_parameter_df(sbml_model     = r_to_py(pepy$load_sbml_from_file(files$modelXML)),
#                          condition_df   = r_to_py(pepy$get_condition_df(files$experimentalCondition)),
#                          observable_df  = r_to_py(pepy$get_observable_df(files$observables)),
#                          measurement_df = r_to_py(pepy$get_measurement_df(files$measurementData)))
# 
# #python code
# # import petab
# # petab.petab_create_parameter_df(sbml_model    = petab.get_sbml_model("petab/enzymeKinetics/model_enzymeKinetics.xml"),
# #                          condition_df   = petab.get_condition_df("petab/enzymeKinetics/experimentalCondition_enzymeKinetics.tsv"),
# #                          observable_df  = petab.get_observable_df("petab/enzymeKinetics/observables_enzymeKinetics.tsv"),
# #                          measurement_df = petab.get_measurement_df("petab/enzymeKinetics/measurementData_enzymeKinetics.tsv"))
# # 
# # import petab
# # petab.petab_create_parameter_df(sbml_model    = petab.get_sbml_model("petab/Boehm_JProteomeRes2014/model_Boehm_JProteomeRes2014.xml"),
# #                          condition_df   = petab.get_condition_df("petab/Boehm_JProteomeRes2014/experimentalCondition_Boehm_JProteomeRes2014.tsv"),
# #                          observable_df  = petab.get_observable_df("petab/Boehm_JProteomeRes2014/observables_Boehm_JProteomeRes2014.tsv"),
# #                          measurement_df = petab.get_measurement_df("petab/Boehm_JProteomeRes2014/measurementData_Boehm_JProteomeRes2014.tsv"))
# 
# #
# pepy$petab_create_parameter_df(sbml_model = r_to_py(sbmlmodel))


# Exit ----
