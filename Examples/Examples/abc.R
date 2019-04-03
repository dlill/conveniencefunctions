library(conveniencefunctions)
setwd(tempdir())
reactions <- NULL %>%
  addReaction("A", "B", "k1 * A *(kinh * F/(1+F))") %>%
  addReaction("B", "C", "k2 * B") %>%
  addReaction("C", "",  "k3 * C") %>%
  addReaction("F", "", "0")

compiled <- odemodel(reactions, fixed = c("B", "C"), forcings = "F")

forc <- data.frame(name = "F", time = seq(0,10,0.01), value = cos(seq(0,10,0.01)))

x <- Xs(compiled, forcings = forc)
attributes(x)

g <- Y(c(Bobs = "B", "Cobs" = "C"), x, attach.input = F)

trafo <-getParameters(g,x)
