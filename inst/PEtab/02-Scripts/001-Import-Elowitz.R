# -------------------------------------------------------------------------#
# 0 Header ----
# -------------------------------------------------------------------------#
# 001-Import-Elowitz.R
# [PURPOSE]
# [AUTHOR]
# Daniel Lill
# [Date]
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
library(conveniencefunctions)
rm(list = ls(all.names = TRUE))
assign_folders()
# -------------------------------------------------------------------------#
# 1 Import the model ----
# -------------------------------------------------------------------------#
importPEtabSBML(modelname = "Elowitz_Nature2000", path2model = "../00-OriginalModels/") 


# -------------------------------------------------------------------------#
# 2 Fit ----
# -------------------------------------------------------------------------#
myframe <- fitModelPEtabSBML(nrfits = 1)
bestfit <- as.parvec(myframe, 1)


# Exit ----


