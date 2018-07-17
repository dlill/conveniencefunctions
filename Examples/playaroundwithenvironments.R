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









myfn <- function(myfrm = parent.frame()) {
  cat("parent frame calling this function\n")
  print(myfrm)
  cat("same, but made with pos.to.env(-1)\n")
  mypos <- pos.to.env(-1)
  print(mypos)
  bla <- function(myfrm2 = parent.frame()) {
    cat("parent frame calling this function\n")
    print(myfrm2)
    mypos2 <- pos.to.env(-1)
    cat("same, but made with pos.to.env(-1)\n")
    print(mypos2)

    cat("current env?\n")
    cur <- as.environment(-1)
    print(cur)


    myenv <- new.env()
    cat("new env made in 2nd fn\n")
    print(myenv)
    cat("parent of new env\n")
    print(parent.env(myenv))
    cat("grand parent of new env\n")
    print(parent.env(parent.env(myenv)))

  }
  bla()
}
myfn()



understand_envirs()

