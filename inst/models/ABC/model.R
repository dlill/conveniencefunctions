# Setup of R session ----
setwd("inst/models/ABC/")
library(dMod)
library(conveniencefunctions)

# Define ODEs ----

# Setup reactions
f <- NULL %>%
  addReaction("A", "B", "k1*A", "Rate 1") %>%
  addReaction("B", "C", "k2*B", "Rate 2") %>%
  addReaction("C", "",  "k3*C", "Rate 3")

# Setup Events
events <- NULL %>%
  addEvent(var = "A", time = "5", value = "A_event", method = "add")

# Write and compile the model in C
c_model <- odemodel(f,
                  events = events,
                  fixed = c("B", "C"),
                  modelname = "ABC"
                  )
remove_c_and_o()                                                     # clean working directory: keep only compiled files

# Generate prediction function
x <- Xs(c_model)


# Define observables ----
observables <- eqnvec("Aobs" = "scale_A*A",
                      "Cobs" = "scale_C*C")
# Generate observation function
g <- Y(observables, x)

# Simulate data with two different conditions ----
pars_data <- data.frame(
  k1     	=	1	, # 1
  k2     	=	1	, # 2
  k3     	=	c(1,0)	, # 3
  A_event	=	5	, # 4
  scale_A	=	1	, # 5
  scale_C	=	.5	, # 6
  A      	=	c(10,5)	, # 7
  B      	=	0	, # 8
  C      	=	0	  # 9
)

times_data <- seq(0,10,2)

set.seed(1)
condition1 <-
  (g*x)(times_data, pars_data %>% map_dbl(1), deriv = F) %>%
  .[[1]] %>%
  .[,1:3] %>%
  as.data.frame() %>%
  gather(name, value, 2,3) %>%
  mutate(sigma = 0.1*value + .Machine$double.eps, value = value + map_dbl(sigma, . %>% rnorm(1,0,.)))

condition2 <-
  (g*x)(times_data, pars_data %>% map_dbl(2), deriv = F) %>%
  .[[1]] %>%
  .[,1:3] %>%
  as.data.frame() %>%
  gather(name, value, 2,3) %>%
  mutate(sigma = 0.1*value + .Machine$double.eps, value = value + map_dbl(sigma, . %>% rnorm(1,0,.)))

data <- datalist(C1 = condition1, no_degradation = condition2)

# Define conditions via parameter transformation, generate parameter transformation function ----
p <- getParameters(g,x) %>%
  setNames(.,.) %>%                                           # symbolic identity trafo: "name <- value"
  insert("x~0", x = c("B", "C")) %>%                          # insert some values
  branch(covariates(data)) %>%                                # generate condition specific parameter trafos
  insert("x~x_cond", x = c("A", "k3"), cond = condition) %>%  # set condition specific parameters
  insert("x~exp(x)", x = getSymbols(mytrafo[[i]])) %>%        # work with logparameters
  P()



# Build dMod.frame ----

model <- dMod.frame("Hypothesis 1", g, x, p, data, f = list(f))
saveRDS(model, "model.rds")








