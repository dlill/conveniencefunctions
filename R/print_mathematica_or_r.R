# Mathematica ----

#' Print ode vector in mathematica format
#'
#' @param myeqnlist an eqnlist
#'
#' @export
#'
#' @examples
#' NULL %>%
#'   addReaction("A", "B", "k1*A", "conversion A to B") %>%
#'   addReaction("B", "", "k2*B*A", "dedradation of B induced by A") %>%
#'   print_mathematica.eqnlist
print_mathematica.eqnlist <- function(myeqnlist) {myeqnlist %>%
    as.eqnvec() %>%
    print_mathematica.eqnvec}

#' Print ode vector in mathematica format
#'
#' @param myeqnvec an eqnvec
#'
#' @export
#'
#' @examples
#' NULL %>%
#'   addReaction("A", "B", "k1*A", "conversion A to B") %>%
#'   addReaction("B", "", "k2*B*A", "dedradation of B induced by A") %>%
#'   as.eqnvec %>%
#'   print_mathematica.eqnlist
print_mathematica.eqnvec <- function(myeqnvec) {
  myeqnvec %>%
  {structure(as.character(.), names = names(.))} %>%
    .[order(names(.))] %>%
    paste(collapse = ", ")  %>%
    str_replace_all(c("_" = "")) %>%
    paste0("f={",.,"};")}


#' Print a character in mathematica format
#'
#' @param mycharacter
#'
#'
#'
#' @export
#'
#' @examples
#'  print_mathematica.character(paste0(letters, "_", letters))
print_mathematica.character <-  function(mycharacter) {
  list(
    mycharacter %>%
      paste(collapse = ", ")  %>%
      str_replace_all(c("_" = "")) %>%
      paste0("x={",.,"};")
    ,
    structure(mycharacter,
              names = paste0("\\b", mycharacter %>%
                               str_replace_all(c("_" = "")), "\\b"))
  )
}

#' Print the values such that they are assignments in mathematica
#'
#' @param vec
#'
#'
#' @export
#'
#' @examples
#' print_mathematica.name_equals_value(c(a = 1, b = 2))
print_mathematica.name_equals_value <- function(vec) {vec %>% paste0(names(.), " = ", . , ";\n") %>% paste0(collapse = "") %>% cat}




#' Print mathematica
#'
#' @param mat
#'
#'
#' @export
#'
#' @examples
#' print_mathematica.matrix(diag(1:5))
print_mathematica.matrix <- function(mat, name_in_mathematica = "M") {
  mat %>%
    apply(1, function(myrow) {
    paste0("{", paste0(myrow, collapse = ", "), "}")
  }) %>%
  {paste0(name_in_mathematica, " = {", paste0(., collapse = ", "), "};")} %>%
  str_replace_all(c("_" = "")) %>%
 {.}
}


# R ----

#' nicely formatted dput for named vectors
#'
#' @param myvec
#'
#'
#' @export
#'
#' @examples
#' letters[1:5] %>% are_names_of(0) %>% print_r.named_vector
print_r.named_vector <- function(myvec, indices = T) {

  if(!indices) {
    myvec %>%
      paste(str_pad(names(.), width = max(str_length(names(.))), side = "right"), "=", ., sep = "\t") %>%
      paste0(collapse = "\t,\n") %>% paste("c(\n",.,"\n)") %>%
      cat
  } else {
    n <- length(myvec)
    myvec %>%
      paste(str_pad(names(.), width = max(str_length(names(.))), side = "right"), "=", ., sep = "\t") %>%
      paste0("\t, # ", 1:n) %>%
      paste0(collapse = "\n") %>%
      paste("c(\n",.,"\n)") %>%
      str_replace(paste0(", # ",n), paste0("  # " , n)) %>%
      cat
  }


}