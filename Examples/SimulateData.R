
  # Write your example here. You can also add more Start..End blocks if needed.
  # Please mask all output such as print() with the special tag
  #    
  # such that the test is not littered. Statements guarded by  are enabled
  # in the example file which is extracted from this test file. To extract the
  # example run
  #    extractExamples()
  # on the R command line.

   library(conveniencefunctions)

  f <- NULL %>%
    addReaction("A", "B", "kcat*A*E/(Km + A)", "Activation") %>%
    addReaction("B",  "", "k2*B", "Degradation") %>%
    addReaction("E", "", "0", "Added Enzyme") %>%
    as.eqnvec()

  events <- eventlist(var = "E", time = 2, value = "E_add", method = "add")
  setwd(tempdir())
  x <- odemodel(f, events = events) %>% Xs
  g <- Y(c(Bobs = "s1*B"), x)

  covtable <- data.frame(membrane = rep(1:2, each = 4), dose = rep(c(0,0.1,1,10), 2)) %>% as.condition.grid()
  parameters <- getParameters(g,x)

  trafo <-
    parameters %>%
    setNames(.,.) %>%
    branch(covtable) %>%
    insert("E~0") %>%
    insert("B~0") %>%
    insert("x~x_scale", x = "s1", scale = membrane) %>%
    insert("x~dose", x = "E_add", dose = dose) %>%
    insert("x~exp(log(10)*x)", x = getSymbols(mytrafo[[i]])) %>%
    {.}

  p <- P(trafo)

  pars0 <- c(
    kcat	=	0	, # 1
    Km  	=	2	, # 2
    k2  	=	-1	, # 3
    s1_1	=	0	, # 4
    A   	=	2	, # 5
    s1_2	=	1	  # 7
  )


  model <- dMod.frame("1", g,x,p, NULL, NULL, pars = list(pars0), covtable = list(covtable)) %>%
    simulate_data(timesD = c(0,5,10), seed_data = 1, seed_pars = 1) %>%
    appendObj(pars = list(pars))

   plot_dr <- plotCombined(model, 1, 1, time == 5, aesthetics = list(x = "dose", group = "membrane", color = "as.factor(membrane)"))
   plot_dr
   plot_classic <- plotCombined(model, 1,1)
   plot_classic

