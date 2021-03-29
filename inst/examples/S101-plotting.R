.tempdir = tempdir()
.currentdir <- getwd()
setwd(.tempdir)

library(conveniencefunctions)

# -------------------------------------------------------------------------#
# 1 Paginate ----
# -------------------------------------------------------------------------#
# Large facetted plot
pl <- ggplot(diamonds) +
  geom_point(aes(carat, price), alpha = 0.1) +
  facet_grid_paginate(color ~ clarity, ncol = 3, nrow = 3, page = 4)

# Basic stuff
n_pages(pl)
getPaginateInfo(pl)
plotlist <- cf_applyPaginate(pl)
length(plotlist)

# Asynchronous outputting
cf_outputFigure(pl, filename = "wup.png")
cf_outputFigure(pl, filename = "wupwup.pdf")

# Go to the files
try(system("nautilus .", wait = FALSE))

# Exit ----
setwd(.currentdir)
future::plan("sequential")
