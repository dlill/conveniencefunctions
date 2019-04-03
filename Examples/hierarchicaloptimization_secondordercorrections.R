
# ###############################################
#
# run this script only on the dMod branch "analytic_hessians"
#
# ###############################################

library(dMod)
library(conveniencefunctions)
setwd(tempdir())

## Model definition (text-based, scripting part)
f <- NULL %>%
  addReaction("A", "B", "k1*A", "translation") %>%
  addReaction("B",  "", "k2*B", "degradation") %>%
  as.eqnvec()


events <- eventlist(var = "A", time = 5, value = "A_add", method = "add")

x <- odemodel(f, events = events) %>% Xs

g <- Y(c(Bobs = "s1*B"), x, compile = T, modelname = "obsfn")

conditions <- c("a", "b")

# there is a bug in
# getParameters(g*x)
parameters <- union(getParameters(g), getParameters(x))

trafo <-
  NULL %>%
  define("x~x", x = parameters) %>%
  branch(conditions = conditions) %>%
  insert("x~x_cond", x = "s1", cond = condition) %>%
  insert("x~1", x = "added", conditionMatch = "a") %>%
  insert("x~5", x = "added", conditionMatch = "b") %>%
  insert("x~exp(x)", x = getSymbols(mytrafo[[i]])) %>%
  {.}

p <- P(trafo)

# Parameter transformation for steady states
pSS <- P(f, method = "implicit", condition = NULL, compile = T)




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


# construct dMod.frame
# set.seed(1)
myframe1 <- dMod.frame("no steady states", g, x, p, data) %>% appendObj()


checkout_hypothesis(myframe1,1)
analytic_parms <- paste0("s1_", letters[1:2])
dynamic_parms <- names(pars) %>% .[!.%in%analytic_parms]


# obj <- normL2(data, prd,log_scale = analytic_parms)
obj <- normL2(data,prd)

mypars <- pars
# optimize the analytic pars
out_ana <- trust(objfun = obj, mypars[analytic_parms],
                 10,10, blather = T,
                 fixed = mypars[dynamic_parms])
mypars[analytic_parms] <- out_ana$argument

# plot argpath
argpath <- out_ana$argpath %>% as.data.frame() %>% setNames(analytic_parms)
argpath %>% ggplot(aes(s1_a,s1_b))+geom_point()

argpath %>% nrow


mypars["s1_a"] <- mypars["s1_a"] %>% `+`(1)

plotCombined(prd(times, mypars), data)

# observations
# 1. adding the analytic second order terms makes optimization worse?
# 2. there are some cases, where the scaling doesn't make sense: when the initial prediction is too bad
# 3.









