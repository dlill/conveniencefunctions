# The goal of this document is to find out if it's possible to quickly find out if it can be done that the
# ode-model is integrated only once when you have the same innerpars of x() for different conditions
#
# This might the case when there are diffent

library(conveniencefunctions)
setwd(tempdir())

f <- NULL %>%
  addReaction("A", "B", "k1*A", "translation") %>%
  addReaction("B",  "", "k2*B", "degradation") %>%
  as.eqnvec()


events <- eventlist(var = "A", time = 5, value = "A_add", method = "add")

x <- odemodel(f, events = events) %>% Xs()
g <- Y(c(Bobs = "s1*B"), x, compile = T, modelname = "obsfn")

conditions <- c("a", "b", "c", "d")
parameters <- union(getParameters(g), getParameters(x))


trafo <-
  NULL %>%
  define("x~x", x = parameters) %>%
  branch(conditions = conditions) %>%
  insert("x~x_cond", x = "s1", cond = condition) %>%
  insert("x~x_cond", x = "k1", cond = condition, conditionMatch = "[cd]") %>%
  insert("x~exp(x)", x = getSymbols(mytrafo[[i]])) %>%

  {.}

p <- P(trafo)

pars <- getParameters(p) %>% are_names_of(1)
pars["k1_d"] <- 2
times <- 0:10


saveints <- (x*p)(times, pars, doSaveIntegrations = T)
dontsaveints <- (x*p)(times, pars)

identical(saveints, dontsaveints)

map(1:4, function(i) {
  identical(saveints[[i]], dontsaveints[[i]])
})

# saveints %>% .[["c"]] %>% attr("par") %>% attr("der")
# dontsaveints %>% .[["c"]] %>% attr("par") %>% attr("der")

map(1:length(attributes(saveints[[3]])), function(i) {
  identical((saveints[[3]] %>% attributes() %>% .[[i]] ), ((dontsaveints[[3]] %>% attributes() %>% .[[i]] )))
})


(g*(x*p))(times, pars, doSaveIntegrations = T) #works, saves integrations
(g*(x*p))(times, pars) # works, saves integrations

(g*x*p)(times, pars) #works but integrates four times
(g*x*p)(times, pars, doSaveIntegrations = T) # doesn't work



# mystep <- si[3]

# dP <- deriv1 <- si[[3]] %>% attr("par") %>% attr("der")
# outSens <- sens2 <- si[[3]] %>% attr("sens")
# deriv <- si[[3]] %>% attr("der")

# chainrule <- function(sens2, deriv1) {
#   time <- sens2[,1, drop = F]
#   sens2 <- sens2[,-1]
#   if("time" %in% colnames(deriv1))
#     stop("pars time dependent")
#
#   sens2Names <- do.call(rbind, strsplit(colnames(sens2), "\\."))
#   myorder <- order(sens2Names[,2],sens2Names[,1])
#
#   sensLong <- matrix(sens2[,myorder], ncol = length(unique(sens2Names[,2])))
#
#   outDeriv <-  sensLong %*% deriv1[unique(sens2Names[myorder,2]),]
#   outDerivGrid <- expand.grid(unique(sens2Names[myorder,1]), colnames(deriv1))
#   outDerivNames <- paste(outDerivGrid[,1], outDerivGrid[,2], sep=".")
#   outDeriv <- matrix(outDeriv, ncol = length(outDerivNames), dimnames = list(NULL, outDerivNames))
#   cbind(time, outDeriv)
# }



# variables <- intersect(variables, names)
# sensLong <- matrix(outSens[,sensNames], nrow = dim(outSens)[1]*length(variables))
# dP <- attr(pars, "deriv")
# if (!is.null(dP)) {
#   sensLong <- sensLong %*% submatrix(dP, rows = c(svariables, sparameters))
#   sensGrid <- expand.grid.alt(variables, colnames(dP))
#   sensNames <- paste(sensGrid[,1], sensGrid[,2], sep = ".")
# }
# myderivs <- matrix(0, nrow = nrow(outSens), ncol = 1 + length(sensNames), dimnames = list(NULL, c("time", sensNames)))
# myderivs[, 1] <- out[, 1]
# myderivs[, -1] <- sensLong





