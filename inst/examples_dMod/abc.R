library(dMod)
.tempdir = tempdir()
.currentdir <- getwd()
setwd(.tempdir)

f <- addReaction(NULL, from = "", to = "A", rate = "k1", description = "production")
f <- addReaction(f   , from = "A", to = "B", rate = "k2*A", description = "degradation")
f <- addReaction(f   , from = "B", to = "C", rate = "k3*B", description = "degradation")
f <- addReaction(f   , from = "C", to = "0", rate = "k3*C", description = "degradation")
odes <- as.eqnvec(f)
events  <- eventlist(c("A"), c(0,0,4), value = c(10, 3 ,0), method = "replace")
events <- sample_n(events, 3)
events
compiled_model <- odemodel(odes, events = events)
x <- Xs(compiled_model)

pars <- setNames(rep(1, length(getParameters(x))), getParameters(x))
pars["A"] <- 0

times <- sort(c(seq(0,10,length.out = 1000), 0.000000001, 0))

plot(x(times, pars))
x(times, pars)[[1]] %>% head
