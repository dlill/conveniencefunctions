# -------------------------------------------------------------------------#
# 0 Breast Cancer Model HER2Low FINAL ----
# -------------------------------------------------------------------------#
#
# M00-ExampleModel.R
#
# [PURPOSE]
# 
# Define model for HER2Low Breast Cancer cells MCF7 and T47D
# under stimulation with different ligand-drug combinations
#
#
# [AUTHOR]
# model by Svenja Kemmer
# data by Aileen Reinz and Mireia Berdiel
#
# [Date]
# 2020-04-02 
#
rm(list = ls(all.names = TRUE))
try(setwd(dirname(rstudioapi::getSourceEditorContext()$path)))
.currentwd <- getwd()
.build <- TRUE
.outputFolder     <- "../04-Output/M00-ExampleModel/"
.dataFolder       <- "../01-ExampleData"
.modelFolder      <- file.path(.outputFolder, "01-Model")

# .. Load Libraries -----
library(dMod)
library(deSolve)
library(rootSolve)
library(dplyr)
library(tidyverse)
library(dplyrExtras)
library(conveniencefunctions)
library(parallel)

# -------------------------------------------------------------------------#
# 1 Load data ----
# -------------------------------------------------------------------------#

mydata <- fread(file.path(.dataFolder, "ExampleData.csv")) %>% 
  as.datalist(split.by = c("inhibitor", "sample", "ligand"))
  
# -------------------------------------------------------------------------#
# 2 Model definition ----
# -------------------------------------------------------------------------#

# .. 1 Observables -----
observables <- eqnvec(
  pAKT_obs = "scale_pAkt*pAKT + offset_pAkt"
)

# .. 2 Reactions -----
reactions <- NULL %>% 
  addReaction("AKT", "pAKT", rate = "(phospho_AKT+phospho_AKT_add*mutation_on)*AKT*ligand", description = "AKT to pAKT by R1R1 (NISS)") %>% 
  addReaction("pAKT", "", rate = "(dephospho_AKT+dephospho_AKT_add*mutation_off)*pAKT", description = "pAKT degradation") %>% 
  {.}

# .... Define ODEs ------ #
myODEs <- as.eqnvec(reactions) %>% unclass() #%>% mat_simplify()

# .. 3 Events -----
# add event pars to reactions
reactions <- addReaction(reactions, "0", "ligand", rate = "0", description = "ligand induction (NISS)") %>% 
  {.}

# create eventlist
events <- NULL %>% 
  addEvent(var = "ligand", time = 60, value = 1, method = "replace") %>% 
  {.}

# .. 4 Errors -----
which_err <- c(1:length(observables))
errors <- paste("sigma_rel", names(observables)[which_err], sep = "_")
names(errors) <- names(observables[which_err])


# .. 5 Parameters -----
# innerpars
parameters_df <- cf_build_parameters_df(myODEs, observables, errors) 

# -------------------------------------------------------------------------#
# 3 Parameter transformation ----
# -------------------------------------------------------------------------#
# .. 1 condition.grid -----
cg <- attr(mydata, "condition.grid") 
condition.grid <- as.data.frame(cg) %>% mutate(condition = rownames(cg)) %>% 
  mutate(ID = 1:nrow(cg)) %>% 
  .[ , c("ID", "condition", "inhibitor", "sample", "ligand")]

myconditions <- condition.grid$condition
rownames(condition.grid) <- myconditions

# .. 2 trafo original -----
innerpars <- unique(c(getParameters(reactions), getSymbols(observables), getSymbols(errors)))
trafo <- define(NULL, "x~y", x = innerpars, y = innerpars) %>% 
  insert("x~-50", x = c("ligand","pAKT")) %>% # exp(-50) ~ 0
  {.}

## SS-Test
# trafo <- repar("x~y", trafo, x = names(CQ_equations), y = CQ_equations)
# trafo <- repar("x~0", trafo, x = c(noSS_pars)) 

# .. 3 trafo list original -----
trafoL <- branch(trafo, condition.grid)

# Specify cell line differences
trafoL <- insert(trafoL, "x~0", # exp(0) = 1
                 x = c("mutation_on", "mutation_off"), 
                 sample == "MCF7") %>% 
  # Specify SS 
  insert("x~x_samp", 
         x =c("AKT", "mutation_on", "mutation_off"), 
         samp = sample) %>% 
  # Specify drug effect
  insert("x~x_drug", 
         x =c("phospho_AKT", "dephospho_AKT"), 
         drug = inhibitor) 

# Specify scale and offset according to experiment/ligand
trafoL <- insert(trafoL, "x~x_lig", 
                 x = c("scale_pAkt", "offset_pAkt") , 
                 lig = ligand)


# .. 4 fixed.grid -----
# select all trafo elements containing numbers
fixed_trafo <- trafo[suppressWarnings(which(!(trafo %>% as.numeric()) %>% is.na()))] %>% as.eqnvec()
fixed_df <- data.frame(as.list(fixed_trafo), stringsAsFactors=FALSE) 

fixed_df2 <- fixed_df %>% rbind(fixed_df[rep(1, (length(myconditions)-1)), ]) %>% 
  cbind(condition = myconditions, ID = 1:length(myconditions)) %>% 
  select(ID, condition, everything()) 

# add FLAGfixed to parameter_df
fixedpars <- names(fixed_trafo)
parameters_df <- parameters_df %>% 
  mutate(FLAGfixed = case_when(name0 %in% fixedpars ~ TRUE, TRUE ~ FALSE))


# .. 5 est.grid -----
estpars <- setdiff(names(trafo), names(fixed_trafo))
est_trafo <- trafo[estpars]
est_df <- NULL
for(cond in myconditions){
  est_row <- data.frame(lapply(trafoL[[cond]][estpars], as.character), stringsAsFactors=FALSE) %>% 
    mutate(condition = cond)
  est_df <- rbind(est_df, est_row)
}

# filter fixedpars from est.grid 
partly_fixed_df <- est_df %>% select_if(function(x) any(x %in% c("0", "1"))) 
partly_fixed_df[partly_fixed_df != "0" & partly_fixed_df != "1"] <- NA
# append to fixed.grid
fixed.grid <- fixed_df2 %>% cbind(partly_fixed_df) %>% as_tibble()

# assign "dummy" to fixed pars
est_df[est_df == "0"] <- "dummy"
est_df[est_df == "1"] <- "dummy"

est.grid  <- est_df %>% mutate(ID = 1:length(myconditions)) %>% 
  select(ID, condition, everything()) %>% as_tibble()

# .. 6 trafo p -----
trafoP <- define(NULL, "x~y", x = innerpars, y = innerpars) %>% 
  # insert("x~y", x = names(mySS_eqns_est), y = mySS_eqns_est) %>% 
  insert("x ~ exp(x)", x = .currentSymbols) %>%
  {.}

# .. 7 pouter -----
outerpars <- unlist(est.grid[, setdiff(names(est.grid), c("ID", "condition"))]) %>% unique()
# remove "dummy" :)
outerpars <- outerpars[outerpars != "dummy"] 
pouter <- structure(rnorm(length(outerpars)), names = outerpars)

# -------------------------------------------------------------------------#
# 4 Build dMod objects ----
# -------------------------------------------------------------------------#
events <- as.eventlist(events)
events$root <- NA
if (.build){
  setwd(.modelFolder)
  
  # .. 1.1 Generate ODEmodel ----- 
  x <- odemodel(reactions, 
                fixed = c("ligand"), 
                modelname = "x", 
                events = events,
                compile = FALSE) %>% Xs()
  
  # .. 2 Generate Observation function -----
  g <- Y(observables, f = reactions, condition = NULL,
         compile = F, modelname = "g")
  
  # .. 3 Define error model -----
  e <- Y(errors, f = c(as.eqnvec(reactions), observables), states = names(observables), 
         compile = F, modelname = "e", attach.input = FALSE)
  
  # .. 4 Define parameter trafo -----
  p <- P(trafoP, modelname = "p")
  
  # .. 5 Compile and Export -----
  soname <- str_remove_all(paste0(basename(.outputFolder)), c("-|_"))
  compile(x, g, p, e, output = soname, cores  = detectFreeCores())
  save(x, g, p, e, file = "001-model.rds")
  setwd(.currentwd)
  
} else {
  # .. 1.2 Load DLLs -----
  setwd(.modelFolder)
  load("001-model.rds")
  loadDLL(x)
  setwd(.currentwd)
}

# pinner <- setNames(rep(exp(-1), length(getParameters(x))),getParameters(x))
# x(seq(0,1,0.1), pinner)

# -------------------------------------------------------------------------#
# 5 Build prd & obj ----
# -------------------------------------------------------------------------#

times <- seq(0,250,1)
prd0 <- (g*x*p)
prd <- cf_PRD_indiv(prd0, est.grid, fixed.grid)
prd(times, pouter, FLAGbrowser = T)
obj <- cf_normL2_indiv(mydata, prd0, e, est.grid, fixed.grid)
obj(pouter)

prediction <- prd(times, pouter)
plotCombined(prediction, mydata)

# -------------------------------------------------------------------------#
# 6 Fit model ----
# -------------------------------------------------------------------------#

# .. 1 mstrust -----
if(FALSE){
  out <- mstrust(obj, pouter, rinit = 1, rmax = 10, 
                 sd = 3, parupper = 12, parlower = -12, 
                 fits = 20, cores = detectFreeCores(), fixed = NULL)
  
  fitlist <- as.parframe(out)
  plotValues(fitlist)
  bestfit <- as.parvec(fitlist,1)
  
  prediction <- prd(times, bestfit)
  plotCombined(prediction, mydata)
}

# .. 2 profiles -----
if(FALSE){
  profiles <- NULL
  profiles <- do.call(
    rbind, 
    lapply(1:2, function(i) {
      profile(obj, bestfit, whichPar = i, limits = c(-3, 3),
              method = "integrate")
    })
  )
  plotProfile(profiles)
  
  
}
# -------------------------------------------------------------------------#
# Use normIndiv ----
# -------------------------------------------------------------------------#
if(FALSE){
  # .. 1 Transpose fixed.grid -----
  fixed.gridT <- fixed_df2 %>% select(-"ID") %>% as.data.table()
  fixed.gridT <- dcast(melt(fixed.gridT, id.vars = "condition", variable.name = "parname"), parname ~ condition)
  fixed.gridT[, `:=`(partask = parname)]
  setcolorder(fixed.gridT, c("parname", "partask"))
  
  # .. 2 Define non-conditional pouter -----
  outerpars.raw <- setdiff(names(est.grid), c("ID", "condition"))
  pouter.raw <- structure(rep(-1, length(outerpars.raw)), names = outerpars.raw)
  
  # .. 3 Try normIndiv -----
  obj <- normIndiv(data = mydata, 
                   prd0 = prd0, 
                   errmodel = e,
                   iiv = NULL,
                   conditional = NULL,
                   fixed.grid = fixed.gridT)
  # obj(pouter.raw)
}




# Exit ----


