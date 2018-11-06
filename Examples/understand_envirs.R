#' Function to understand environments
#'
#' @export
#' @examples
#' map(1, function(i) {
#' mnv <- new.env()
#' cat("current position")
#' print(parent.env(mnv))
#' bla <- understand_envirs()
#' bla()
#' })
understand_envirs <- function() {
   bla <- function(myfrm2 = parent.frame()) {
      # cat("parent frame calling this function\n")
      # print(myfrm2)
      # mypos2 <- pos.to.env(-1)
      # cat("same, but made with pos.to.env(-1)\n")
      # print(mypos2)
      #
      # cat("current env? no, also parent frame\n")
      # cur <- as.environment(-1)
      # print(cur)


      myenv <- new.env()
      # cat("new env made in 2nd fn\n")
      # print(myenv)
      # cat("parent of new env\n")
      # print(parent.env(myenv))
      # cat("grand parent of new env\n")
      # print(parent.env(parent.env(myenv)))
      # cat("great grand parent of new env\n")
      # print(parent.env(parent.env(parent.env(myenv))))
      # cat("great^2 grand parent of new env\n")
      # print(parent.env(parent.env(parent.env(parent.env(myenv)))))
      # cat("great^3 grand parent of new env\n")
      # print(parent.env(parent.env(parent.env(parent.env(myenv)))))
      # cat("great^4 grand parent of new env\n")
      # print(parent.env(parent.env(parent.env(parent.env(parent.env(parent.env(myenv)))))))

      list("new env made in 2nd fn\n"=
                 myenv,
                 "parent of new env\n"=
                 parent.env(myenv),
                 "grand parent of new env\n"=
                 parent.env(parent.env(myenv)),
                 "great grand parent of new env\n"=
                 parent.env(parent.env(parent.env(myenv))),
                 "great^2 grand parent of new env\n"=
                 parent.env(parent.env(parent.env(parent.env(myenv)))),
                 "great^3 grand parent of new env\n"=
                 parent.env(parent.env(parent.env(parent.env(myenv)))),
                 "great^4 grand parent of new env\n"=
                 parent.env(parent.env(parent.env(parent.env(parent.env(parent.env(myenv))))))
           )

    }

}










#' Title
#'
#' @export
#'
#' @examples
#' lapply(1, function(i) {
#' understand_parentframes()
#' })
understand_parentframes <- function() {

  cat("parent.frame\n")
  pf <- parent.frame()
  print(pf)

  inner_fn <- function() {

    pf_inner <- parent.frame()
    cat("inner parent.frame\n")
    print(pf_inner)

    pf_inner <- parent.frame()
    cat("parent inner parent.frame\n")
    print((pf_inner %>% parent.env))

    pf_inner <- parent.frame()
    cat("parent^2 inner parent.frame\n")
    print((pf_inner %>% parent.env %>% parent.env))

    pf_inner <- parent.frame()
    cat("parent^3 inner parent.frame\n")
    print((pf_inner %>% parent.env %>% parent.env %>% parent.env))

  }
  inner_fn()
}



