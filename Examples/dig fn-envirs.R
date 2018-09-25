prior.frame <- readRDS("~/Promotion/Projects/methacetin_fitting/Fit_model_41/2018_04_02_22_37_prior.frame.rds")

p <- prior.frame$p_0[[1]]
g <- prior.frame$g[[1]]


ls(envir = environment(p))

ls(envir = environment(get("p", envir = environment(get("x1", envir = environment(p))))))

split.fn <- function(fn) {
  list(x1 = get("x1",envir = environment(fn)),
       x2 = get("x2",envir = environment(fn)))
}




split(p)[[1]] %>% environment() %>% ls(envir = .)


ls(envir = environment(split(p)[[2]]))

get("p2p", envir = environment(split(p)[[2]]))


ls(envir = environment(get("p2p", envir = environment(split(p)[[2]]))))


get("modelname", envir = environment(get("p2p", envir = environment(split(p)[[2]]))))


get("modelname", envir = environment(get("X2Y", envir = environment(g)) ))

ls(envir = environment( get("gEval", envir = environment(get("X2Y", envir = environment(g)) ))))

get("funcname", envir = environment( get("gEval", envir = environment(get("X2Y", envir = environment(g)) ))))
