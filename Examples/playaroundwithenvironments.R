x <- "wup"
myfunfac <- function(x = "wup") {
  return(function(bla) paste0(x,bla))
}
myfun <- myfunfac()

ls(envir = environment(myfun))

myfun2 <- function(myfun1, ...) {

  print(myfun1(...))
  ls(envir = environment(myfun1))
}

myfun2(myfun, "bla")
