# library(conveniencefunctions)
devtools::load_all("~/Promotion/Promotion/Software/dMod")
devtools::load_all("~/Promotion/Promotion/Projects/conveniencefunctions")
try(setwd(dirname(rstudioapi::getSourceEditorContext()$path)))

# -------------------------------------------------------------------------#
# Create enzyme kinetics model and data ----
# -------------------------------------------------------------------------#
# .. Eqnlist and objects ----- #
modelname <- "AB"

el <- NULL
el <- addReaction(el, from = "", to = "A", rate = "kprodA",
                  description = "production of A")
el <- addReaction(el, from = "A", to = "", rate = "kdegA*A",
                  description = "degradation of A")
el <- eqnlist_addDefaultCompartment(el, "cytoplasm") # Need compartment information for SBML

parInfo <- data.table(tibble::tribble(
  ~parName, ~parValue, ~parUnit,
  "kprodA",     1  ,"mole_per_litre_per_second" ,    # Because of compartment, all dMod-fluxes are multiplied with cytoplasm volume
  "kdegA"  ,    1,"per_second"))

speciesInfo <- data.table(tibble::tribble(
  ~speciesName, ~compName, ~initialAmount,
  "A"         ,"cytoplasm" ,             0))

# compartmentInfo is left as the default getCompartmentInfo(el)
# unitInfo is left as the default getUnitInfo(): If you need other units, you need to add them

# .. Simulate Data ----- #
compiled <- odemodel(f = el,modelname = modelname)
x <- Xs(compiled, condition = "C1")
pars <- c(setNames(parInfo$parValue, parInfo$parName),
          setNames(speciesInfo$initialAmount, speciesInfo$speciesName))

pred <- x(seq(0,10,0.1), pars)
pred <- data.table(as.data.frame(pred))
pred <- pred[time > 0]
pred[,`:=`(sigma = 0.0001)]
# pred <- rbind(pred,pred,pred)
# pred[,`:=`(value = exp(log(value) + rnorm(length(value), sd = sigma)))]
pred[,`:=`(name = paste0("obs", name))]
cfggplot(pred, aes(time, value, color = name)) +
  facet_wrap(~name, scales = "free") + 
  geom_point()

# -------------------------------------------------------------------------#
# Export sbml ----
# -------------------------------------------------------------------------#
sbml_exportEquationList(equationList = el, 
                        filename = "SBML/NULLtoAtoNULL.xml",
                        modelname = "NULLtoAtoNULL",
                        parInfo = parInfo,
                        speciesInfo = speciesInfo)


# Exit ----
