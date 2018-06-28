# Setup of R session ----
setwd("inst/models/ABC/")
library(dMod)
library(conveniencefunctions)


# Load dMod.frame ----
model <- readDMod.frame("model.rds")

# Expand dMod.frame by derived objects
model <- model %>% appendObj()

myframe <- dMod.frame(hypothesis, Id(), x,p, data)

