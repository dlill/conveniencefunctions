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

mydata <- data.frame(name = c("pAKT_obs"), 
                     sample = c("T47D", "MCF7"),
                     inhibitor = c("no_inhibitor", "no_inhibitor", "lumretuzumab", "lumretuzumab"),
                     ligand = ("HRG"),
                     time = c(10),
                     value = c(0.6, 2.3, 0.3, 1.2),
                     sigma = c(NA)) %>% 
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
  addReaction("AKT", "pAKT", rate = "(phospho_AKT_R1R1*mutation_on)*AKT*ligand", description = "AKT to pAKT by R1R1 (NISS)") %>% 
  addReaction("pAKT", "AKT", rate = "dephospho_pAKT*pAKT*mutation_off", description = "pAKT to AKT") %>% 
  {.}

# .... Define ODEs ------ #
myODEs <- as.eqnvec(reactions) %>% unclass() %>% mat_simplify()

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
condition.grid <- cg %>% mutate(condition = rownames(cg)) %>% 
  mutate(ID = 1:nrow(cg)) %>% 
  .[ , c("ID", "condition", "inhibitor", "sample", "ligand")]

myconditions <- condition.grid$condition
rownames(condition.grid) <- myconditions

# .. 2 trafo original -----
innerpars <- unique(c(getParameters(reactions), getSymbols(observables), getSymbols(errors)))
trafo <- define(NULL, "x~y", x = innerpars, y = innerpars) %>% 
  insert("x~0", x = c("ligand","pAKT")) %>% 
  {.}

## SS-Test
# trafo <- repar("x~y", trafo, x = names(CQ_equations), y = CQ_equations)
# trafo <- repar("x~0", trafo, x = c(noSS_pars)) 

# .. 3 trafo list original -----
trafoL <- branch(trafo, condition.grid)

# Specify cell line differences
trafoL <- insert(trafoL, "x~1", 
                 x = c("mutation_on", "mutation_off"), 
                 sample == "MCF7") %>% 
  # Specify SS 
  insert("x~x_samp", 
         x =c("AKT", "mutation_on", "mutation_off"), 
         samp = sample)

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

# .. 7 est.vec -----
outerpars <- unlist(est.grid[, setdiff(names(est.grid), c("ID", "condition"))]) %>% unique()
# remove "dummy" :)
outerpars <- outerpars[outerpars != "dummy"] 
pouter <- structure(rep(-1, length(outerpars)), names = outerpars)

# -------------------------------------------------------------------------#
# Build dMod objects ----
# -------------------------------------------------------------------------#

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
  soname <- str_remove_all(paste0(basename(.outputFolder),".so"), c("-|_"))
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

prd0 <- (g*x*p)
prd <- cf_PRD_indiv(prd0, est.grid, fixed.grid)
prd(seq(0,1,0.1), pouter, FLAGbrowser = F)
obj <- cf_normL2_indiv(mydata, prd0, e, est.grid, fixed.grid)
obj(pouter)

# Exit ----


