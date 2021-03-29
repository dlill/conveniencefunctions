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

# library(conveniencefunctions)
devtools::load_all("~/Promotion/Promotion/Projects/conveniencefunctions")
rm(list = ls(all.names = TRUE))

.outputFolder <- paste0("../04-Output/", "S101-plotting")
for(folder in c(.outputFolder)) 
if(!dir.exists(folder)) dir.create(folder, recursive = TRUE)

# -------------------------------------------------------------------------#
# 1 Paginate ----
# -------------------------------------------------------------------------#
# pl <- ggplot(diamonds) +
#   geom_point(aes(carat, price), alpha = 0.1) +
#   facet_grid_paginate(color ~ clarity, ncol = 3, nrow = 3, page = 4)
pl <- ggplot(mtcars, aes(wt, mpg)) + 
  facet_wrap_paginate(cyl~gear, nrow = 2, ncol = 2) +
  geom_point()
pl
debugonce(getPaginateInfo)
getPaginateInfo(pl)
plotlist <- cf_applyPaginate(pl)
length(plotlist)

debugonce(cf_outputFigure)
cf_outputFigure(pl, filename = "bla.png")
system("nautilus .", wait = FALSE)


# Exit ----
future::plan("sequential")
