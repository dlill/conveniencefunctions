# This is work in progress to have a test-model setup with cf_indiv
# It also reveals the difficulties in setting up a general individualization-language


library(conveniencefunctions)
setwd(tempdir())  

## Model definition (text-based, scripting part)
f <- NULL %>%
  addReaction("A", "B", "k1*A", "translation") %>%
  addReaction("B",  "", "k2*B", "degradation") %>%
  as.eqnvec()
obs <- c(Bobs = "s1*B")

parameters_df <- cf_build_parameters_df(f, obs, NULL)

fixed.grid <- data.frame(ID = c(1:2), condition = letters[1:2], stringsAsFactors = FALSE)
est.grid   <- data.frame(ID = c(1:2), condition = letters[1:2], stringsAsFactors = FALSE) %>% cbind(as.data.frame(t(setNames(parameters_df$name0,parameters_df$name0)), stringsAsFactors = FALSE), stringsAsFactors = FALSE)

# 
# est.grid = est.grid
# parameters_est_df = parameters_df
# parname = "A"
# conditions = c("a", "b")
# condition_column = "condition"
# FLAGdummifyOtherConds = FALSE
# condition.grid = NULL

dummy <- cf_make_condition_specific(est.grid = est.grid, 
                           parameters_est_df = parameters_df, 
                           parname = "A", 
                           conditions = c("a", "b"), 
                           condition_column = "condition",
                           FLAGdummifyOtherConds = FALSE,
                           condition.grid = NULL)

est.grid <- dummy$est.grid
parameters_df <- dummy$est.vec_df

trafo_df <-
  parameters_df %>% 
  filter(name == name0) %>% 
  mutate(estscale = case_when(name %in% names(fixed.grid) ~ "N", TRUE ~ estscale)) %>% 
  mutate(trafo = case_when(estscale == "L" ~ paste0("exp(", name, ")"),estscale == "N" ~ name)) %>% 
  {.}

trafo <- setNames(trafo_df$trafo, trafo_df$name)
# duplicate inits
trafo_inits <- trafo[trafo_df$name[trafo_df$FLAGinitpar]] %>% setNames(.,paste0("init_", names(.)))
trafo       <- c(trafo, trafo_inits)

x <- odemodel(f, compile = FALSE) %>% Xs
g <- Y(obs, x, compile = FALSE, modelname = "obsfn")
p <- P(trafo, compile = FALSE, modelname = "p")
compile(g,x,p, output = "wupwup")


# [] parameters_df could get columns FLAGindividualized and FLAGestimated
pars_est_df <- 
  parameters_df %>% 
  filter(name != "A") %>% 
  mutate(value = case_when(estscale == "L" ~ log(value), estscale == "N" ~ value),
         upper = case_when(estscale == "L" ~ log(upper), estscale == "N" ~ upper),
         lower = case_when(estscale == "L" ~ log(lower), estscale == "N" ~ lower))

pars <- setNames(pars_est_df$value, pars_est_df$name)


# .. 6 Construct objective function -----
prd0 <- (g*x*p)
prd <- cf_PRD_indiv(prd0, est.grid, fixed.grid)

# .. 7 Test dMod functions-----
times <- seq(0,5, length.out = 10)
pars <- setNames(pars_est_df$value, pars_est_df$name)

prs <-  cf_make_pars(pars, est.grid, fixed.grid, 1)
fxd <- prs$fixed
prs <- prs$pars
p(prs, fixed = fxd)
compare(getParameters(p), names(c(prs, fxd)))

prd0(times, prs, fixed = fxd, deriv= TRUE) 
# .... Check out why there is no dynamics in observables ------
prd0(times, prs, fixed = fxd, deriv= FALSE) %>% as.prdlist() %>% plot(data = NULL)


wupwup <- prd(times, pars, deriv = TRUE)
pars0 <- c(B = 1, k1 = 1, k2 = -1, s1 =0, A_a = 0.5, A_b = 2)
wupwup <- prd(times, pars0, deriv = TRUE) %>%as.prdlist() %>%  plot


# debugonce(obj_data); 
obj_data <- cf_normL2_indiv(dl, prd0, e, est.grid, fixed.grid)
wup <- obj_data(pars, FLAGverbose = TRUE, FLAGbrowser = FALSE)
wup <- obj_data(pars, conditions = c("Leijssen1996_NaN"), FLAGverbose = TRUE, FLAGbrowser = TRUE)
is.na(wup$value)
wup %>% attr("con")
# -------------------------------------------------------------------------#
# 3 Test fit ----
# -------------------------------------------------------------------------#
# .. Fit -----
lower <- setNames(pars_est_df$lower, pars_est_df$name)
upper <- setNames(pars_est_df$upper, pars_est_df$name)
fit <- trust(obj_data, pars, 0.1,10, iterlim = 100, parupper = upper, parlower = lower, simcores = 11, printIter = TRUE)



