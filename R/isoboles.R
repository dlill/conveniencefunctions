# # -------------------------------------------------------------------------#
# # initialize ----
# # -------------------------------------------------------------------------#
# COUNT <- 0
# f <- function(x) {sum(x^2)}
# gridmax <- c(x = 1.5, y = 1.5)
# objval <- 1
# 
# tol <- 1e-2
# 
# initialze_isobole <- function(f, objval, gridmax) {
#   f1 <- function(x1) f(c(x1,0)) - objval
#   x1 <- uniroot(f1, c(0,gridmax[1]), tol = tol)
#   f2 <- function(x2) f(c(0,x2)) - objval
#   x2 <- uniroot(f2, c(0,gridmax[2]), tol = tol)
#   COUNT <<- COUNT + x1$iter
#   COUNT <<- COUNT + x2$iter
#   list(c(0,x2$root),c(x1$root,0))
# }
# p <- initialze_isobole(f,objval,gridmax)
# p0 <- p[[1]]
# p1 <- p[[2]]
# 
# get_midpoint <- function(p1,p2, d) {
#   pm <- (p1 + p2)/2
#   m <- -((p2 - p1)[2]/(p2 - p1)[1])
#   b <- pm[2] - m*pm[1]
#   yorthog <- function(x) m*x + b
#   fm <- function(x) f(c(x,yorthog(x))) - objval
#   
#   ym <- f(pm)
#   interval_x1 <- if(ym < objval) c(pm[1], pm[1]+d) else c(pm[1]-d,pm[1]) # can be better
#   extendInt <- if(ym < objval) "upX" else "downX"
#   px <- uniroot(fm, interval_x1, extendInt = extendInt, tol = tol)
#   COUNT <<- COUNT + px$iter
#   c(px$root, yorthog(px$root))
# }
# 
# p <- initialze_isobole(f,objval,gridmax)
# p0 <- p[[1]]
# p1 <- p[[2]]
# p0.5 <- get_midpoint(p0,p1,1)
# p0.25 <- get_midpoint(p0,p0.5,1)
# p0.125 <- get_midpoint(p0,p0.25,1)
# 
# imax <- 4
# N <- (2^4+1)
# iso <- lapply(1:N, function(x) c(1,2))
# iso0 <- initialze_isobole(f,objval,gridmax)
# iso[1] <- iso0[1]
# iso[N] <- iso0[2]
# i <- (1:imax)[[1]]
# i <- 1
# for(i in 1:imax) {
#   diffi <- (N-1)/(2^i)
#   jmid <- seq(1,N,diffi)
#   d <- gridmax[1] / (2^i)
# j <- (seq(2,length(jmid),2))[[1]]
#   for (j in seq(2,length(jmid),2)) {
#     iso[[jmid[j]]] <- get_midpoint(iso[[jmid[j-1]]],iso[[jmid[j+1]]],d)
#   }
# }
# 
# plot(do.call(rbind,iso))
# 
# COUNT
# 
# # Exit ----
