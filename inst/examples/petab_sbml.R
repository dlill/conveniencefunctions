# library(conveniencefunctions)
devtools::load_all("~/Promotion/Promotion/Projects/conveniencefunctions")
try(setwd(dirname(rstudioapi::getSourceEditorContext()$path)))
# .. Eqnlist and objects -----
modelname <- "enzymeKinetics"

el <- NULL
el <- addReaction(el, from = "E + S", to = "ES", rate = "(kon)*E*S",
                  description = "production of complex")
el <- addReaction(el, from = "ES", to = "E + S", rate = "koff*ES",
                  description = "decay of complex")
el <- addReaction(el, from = "ES", to = "E + P", rate = "kcat*ES",
                  description = "production of product")
el <- eqnlist_addDefaultCompartment(el, "cytoplasm")

parInfo <- data.table(tibble::tribble(
  ~parName, ~parValue, ~parUnit,
  "kon"   ,     1,"litre_per_mole_per_second" ,
  "koff"  ,     0.1,"per_second" ,
  "kcat"  ,     0.1,"per_second" ))

speciesInfo <- data.table(tibble::tribble(
  ~speciesName, ~compName, ~initialAmount,
  "E"         ,"cytoplasm" ,             1,
  "S"         ,"cytoplasm" ,             100,
  "ES"        ,"cytoplasm" ,             0,
  "P"         ,"cytoplasm" ,             0))



# .. Simulate Data -----
compiled <- odemodel(f = el,modelname = modelname)
x <- Xs(compiled, condition = "C1")
pars <- c(setNames(parInfo$parValue, parInfo$parName),
          setNames(speciesInfo$initialAmount, speciesInfo$speciesName))

pred <- as.data.table(x(seq(0,100), pars)[[1]])
pred <- x(seq(0,100, 10), pars)
pred <- data.table(as.data.frame(pred))
pred[,`:=`(sigma = value * 0.1)]
pred[,`:=`(value = value + rnorm(length(value), sd = sigma))]
pred[,`:=`(name = paste0("obs", name))]

# .. Create petab tables -----
pe_ex <- petab_experimentalCondition("C1", "C1")
pe_ob <- petab_observables(observableId = c("obsE","obsS","obsES","obsP"),
                           observableName = c("obsE","obsS","obsES","obsP"),
                           observableFormula = c("E","S","ES","P"), 
                           observableTransformation = "log",
                           noiseFormula = c("0.1*obsE"),
                           noiseDistribution = c("normal"))
pe_me <- petab_measurementData(observableId = pred$name,
                               simulationConditionId = "C1",
                               measurement = pred$value,
                               time = pred$time,
                               observableParameters = NA,
                               noiseParameters = pred$sigma,
                               datasetId = "data1",
                               replicateId = NA,
                               preequilibrationConditionId = NA)
pe_mo <- petab_model(el,events = NULL,parInfo = parInfo, speciesInfo = speciesInfo)

# .. Create petab -----
petab <- petab_init(pe_mo,
                    pe_ex,
                    pe_me,
                    pe_ob)

# debugonce(writePetab)
writePetab(petab, "petab/enzymeKinetics.petab")

# cfoutput_MdTable(getSpeciesInfo(el), NFLAGtribble = 2)
# sbml_exportEquationList(el, filename, parInfo = parInfo)
