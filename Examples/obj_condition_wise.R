\dontrun{
library(dMod)
library(conveniencefunctions)

times <- 0:5
grid <- data.frame(name = "A", time = times, row.names = paste0("p", times))
x1 <- Xd(grid, condition = "C1")

pars <- structure(rep(0, nrow(grid)), names = row.names(grid))

## Simulate data
data.list <- lapply(1:3, function(i) {
  prediction <- x1(times, pars + rnorm(length(pars), 0, 1))
  cbind(wide2long(prediction), sigma = 1)
})

data1 <- as.datalist(do.call(rbind, data.list))

## Generate objective function based on data and model
## Then fit the data and plot the result
obj1 <- normL2(data1, x1)

times <- 0:5
grid <- data.frame(name = "A", time = times, row.names = paste0("p", times))
x2 <- Xd(grid, condition = "C2")

pars <- structure(rep(0, nrow(grid)), names = row.names(grid))

## Simulate data
data.list <- lapply(1:3, function(i) {
  prediction <- x2(times, pars + rnorm(length(pars), 0, 1))
  cbind(wide2long(prediction), sigma = 1)
})

data2 <- as.datalist(do.call(rbind, data.list))

## Generate objective function based on data and model
## Then fit the data and plot the result
obj2 <- normL2(data2, x2)

obj3 <- obj1 + obj2

# devtools::load_all("~/Promotion/Software/dMod/")

#
# debug(obj3)
obj_condition_wise(obj3,pars)
# undebug(obj3)






obj1 <- constraintL2(c(a = 0), condition = c("C1", "C2"))
obj2 <- constraintL2(c(a = 0.1), condition = c("C2", "C3"), attr.name = "prior2")

obj_condition_wise((obj1+obj2),(c(a = 1)))





}







