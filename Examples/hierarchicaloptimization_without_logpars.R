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
  insert("x~exp(x)", x = getSymbols(mytrafo[[i]]) %>% str_subset_not("s1")) %>% #don't logtransform the scalings
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
myframe1 <- dMod.frame("no steady states", g, x, p, data) %>% appendObj()



checkout_hypothesis(myframe1,1)
analytic_parms <- paste0("s1_", letters[1:2])
dynamic_parms <- names(pars) %>% .[!.%in%analytic_parms]



mypars <- pars
# optimize the analytic pars
out_ana <- trust(objfun = obj, mypars[analytic_parms],
                 10,10, blather = T,
                 fixed = mypars[dynamic_parms])
mypars[analytic_parms] <- out_ana$argument

# plot argpath
argpath <- out_ana$argpath %>% as.data.frame() %>% setNames(analytic_parms)
argpath %>% ggplot(aes(s1_a,s1_b))+geom_point()

# why is the argument path not linear for the linear model?
mygrid <-  expand.grid(seq(-2.2,0,0.1),seq(-3.2,1,0.1)) %>% setNames(analytic_parms)
i <- mygrid[1,] %>% unlist
obj_grid <- apply(mygrid, 1, function(i) {
  mypars <- pars
  i <- unlist(i)
  mypars[names(i)] <- i
  out <- data.frame(t(i), value = obj(mypars, deriv = F)$value)
  return(out)
}) %>%
  do.call(rbind,.)

argpath <- out_ana$argpath %>% as.data.frame() %>% setNames(analytic_parms)
obj_grid %>%
  ggplot(aes(s1_a, s1_b, z=value)) +
  geom_contour() +
  geom_point(aes(x = s1_a, y = s1_b, z = NULL), data = argpath)



mygrid <-  expand.grid(seq(-2.0,-1.75,0.01),seq(-3.1,-2.9,0.01)) %>% setNames(analytic_parms)
i <- mygrid[1,] %>% unlist
obj_grid2 <- apply(mygrid, 1, function(i) {
  mypars <- pars
  i <- unlist(i)
  mypars[names(i)] <- i
  out <- data.frame(t(i), value = obj(mypars, deriv = F)$value)
  return(out)
}) %>%
  do.call(rbind,.)

argpath <- out_ana$argpath %>% as.data.frame() %>% setNames(analytic_parms)
obj_grid2 %>%
  ggplot(aes(s1_a, s1_b, z=log(value))) +
  geom_contour() +
  # geom_point(aes(x = s1_a, y = s1_b, z = NULL), data = argpath)
  geom_blank()

# why aren't they ellipses? -> look at quadratic objfun in log-coordinates
mygrid <-  expand.grid(seq(-2.0,-1.75,0.01),seq(-3.1,-2.9,0.01)) %>% setNames(analytic_parms)
i <- mygrid[1,] %>% unlist
obj_grid2 <- apply(mygrid, 1, function(i) {
  mypars <- pars
  i <- unlist(i)
  mypars[names(i)] <- i
  out <- data.frame(t(i), value = (exp(i[[1]])-exp(-1.86))^2 + (exp(i[[2]])-exp(-2.96))^2)
  return(out)
}) %>%
  do.call(rbind,.)

argpath <- out_ana$argpath %>% as.data.frame() %>% setNames(analytic_parms)
obj_grid2 %>%
  ggplot(aes(s1_a, s1_b, z=log(value))) +
  geom_contour() +
  # geom_point(aes(x = s1_a, y = s1_b, z = NULL), data = argpath)
  geom_blank()

plot3D::contour2D(obj_grid2)

# optimize dynamic pars
out_dyna <- trust(objfun = obj, mypars[dynamic_parms],
                  10,10, blather = T,
                  fixed = mypars[analytic_parms])
mypars[dynamic_parms] <- out_dyna$argument


