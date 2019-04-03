## Model definition (text-based, scripting part)
r <- NULL
r <- addReaction(r, "A", "B", "k1*A", "translation")
r <- addReaction(r, "B",  "", "k2*B", "degradation")

f <- as.eqnvec(r)
observables <- eqnvec(Bobs = "s1*B")

innerpars <- getSymbols(c(names(f), f, observables))
trafo0 <- structure(innerpars, names = innerpars)
trafo0 <- replaceSymbols(innerpars,
                         paste0("exp(", innerpars, ")"),
                         trafo0)

conditions <- c("a", "b")

trafo <- list()
trafo$a <- replaceSymbols("s1", "s1_a", trafo0)
trafo$b <- replaceSymbols("s1", "s1_b", trafo0)

events <- list()
events$a <- data.frame(var = "A", time = 5, value = 1, method = "add")
events$b <- data.frame(var = "A", time = 5, value = 2, method = "add")


## Model definition (generate compiled objects
## and functions from above information)

# ODE model
model <- odemodel(f)

# Observation function
g <- Y(observables, f, compile = TRUE, modelname = "obsfn")

# Parameter transformation for steady states
pSS <- P(f, method = "implicit", condition = NULL)

# Condition-specific transformation and prediction functions
p0 <- x <- NULL
for (C in conditions) {
  p0 <- p0 + P(trafo[[C]], condition = C)
  x <- x + Xs(model, events = events[[C]], condition = C)
}

## Process data

# Data
data <- datalist(
  a = data.frame(time = c(0, 2, 7),
                 name = "Bobs",
                 value = c(.3, .3, .3),
                 sigma = c(.03, .03, .03)),
  b = data.frame(time = c(0, 2, 7),
                 name = "Bobs",
                 value = c(.1, .1, .2),
                 sigma = c(.01, .01, .02))
)

## Evaluate model/data

# Initialize times and parameters
times <- seq(0, 10, .1)
outerpars <- attr(p0, "parameters")
pouter <- structure(rnorm(length(outerpars)),
                    names = outerpars)

# Plot prediction
plot((g*x*p0)(times, pouter))
plot((g*x*pSS*p0)(times, pouter))
plotFluxes(pouter, g*x*p0, times, getFluxes(r)$B)

# Fit data with L2 constraint
constr <- constraintL2(mu = 0*pouter, sigma = 5)
myfit <- trust(normL2(data, g*x*p0) + constr,
               pouter, rinit = 1, rmax = 10)
plot((g*x*p0)(times, myfit$argument), data)

# List of fits
obj <- normL2(data, g*x*p0) + constr
mylist <- mstrust(normL2(data, g*x*p0) + constr,
                  pouter, fits = 10, cores = 4, sd = 4)
plot(mylist)
pars <- as.parframe(mylist)
plotValues(pars)

bestfit <- as.parvec(pars)

# Compute profiles of the fit
profile <- profile(normL2(data, g*x*p0) + constr,
                   bestfit, "k1", limits = c(-5, 5))
plotProfile(profile)

# Add a validation objective function
vali <- datapointL2("A", 2, "mypoint", .1, condition = "a")
# Get validation point parameters
mypoint <- trust(normL2(data, g*x*p0) + constr + vali,
                 c(mypoint = 0), rinit = 1, rmax = 10,
                 fixed = bestfit)$argument
# Compoute profile and plot
profile <- profile(normL2(data, g*x*p0) + constr + vali,
                   c(bestfit, mypoint), "mypoint")
plotProfile(profile)


