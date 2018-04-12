data <- data.frame(
  name = rep(letters[1:2], each = 3),
  value = 1:6,
  time = rep(1:3, 2),
  sigma = 0.1,
  par1 = rep(0:1, each = 3),
  par2 = rep(9:10, each = 3),
  par3 = rep(1:3, each = 2),
  stringsAsFactors = F
)

parameters <- c("a", "b", "par1", "par2", "par3")
pars_to_insert <- c("par1", "par2")
# pars_to_insert <- intersect(getParameters(g*x), names(data)) # this would be the usual way when setting up a model

# trafo <-
  NULL %>%
    define("x~x", x = parameters) %>%
    branch(data %>% as.datalist %>% covariates) %>%

    # Trick 1: Access values from covariates()-Table
    insert("name ~ value", value = unlist(mget(pars_to_insert)), name = pars_to_insert) %>%

    # Apply reparametrization only for specific conditions
    insert("x~x_par3", x = "a", conditionMatch = "^0_9", par3 = par3) %>%

    # Trick 2: Access symbols from condition-specific trafo
    insert("x~exp(X)", x = getSymbols(mytrafo[[i]]), X = toupper(getSymbols(mytrafo[[i]])))

