# -------------------------------------------------------------------------#
# 0 Header ----
# -------------------------------------------------------------------------#
#
# S101-plotting.R
#
# [PURPOSE]
# 
# Try and demonstrate plotting with cfoutputFigure
#
#
# [AUTHOR]
# Daniel Lill
#
# [Date]
# Mon Mar 29 10:24:05 2021
#
.tempdir = tempdir()
.currentdir <- getwd()
setwd(.tempdir)

library(conveniencefunctions)
rm(list = ls(all.names = TRUE))

.outputFolder <- paste0("../04-Output/", "S101-plotting")
for(folder in c(.outputFolder)) 
if(!dir.exists(folder)) dir.create(folder, recursive = TRUE)

# -------------------------------------------------------------------------#
# 1 Paginate ----
# -------------------------------------------------------------------------#
pl <- ggplot(diamonds) +
  geom_point(aes(carat, price), alpha = 0.1) +
  facet_grid_paginate(color ~ cut:clarity, ncol = 3, nrow = 3, page = 4)
plotlist <- cf_applyPaginate(pl)

cf_outputFigure(pl)





# Exit ----
future::plan("sequential")
