#' str with max.level = 0
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
str0 <- function(...) {
  str(..., max.level = 0)
}

#' str with max.level = 1
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
str1 <- function(...) {
  str(..., max.level = 1)
}

#' str with max.level = 2
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
str2 <- function(...) {
  str(..., max.level = 2)
}





#' magrittr-style two sided comparison
#'
#' @param x
#' @param lower_lim
#' @param upper_lim
#'
#' @return
#' @export
#'
#' @examples
cf_is_in_between <- function(x, lower_lim, upper_lim) {
  (x > lower_lim) & (x < upper_lim)
}


#' magrittr-Style Apply an expression that's more complicated
#'
#' Old, I use {.} now...
#'
#' @param x
#' @param expr
#'
#' @return
#' @export
#'
#' @examples
cf_apply_expression <- function(x, ...) {
  myenv <- environment()
  eval(as.expression(substitute(...)), envir = myenv)
}

#' magrittr-Style set attributes
#'
#' @param x
#' @param expr
#'
#' @return
#' @export
#'
#' @examples
set_attributes <- function(x, attribute, value) {
  for(i in 1:length(attribute)) {
    attr(x, attribute[i]) <- value[i]
  }
  return(x)
}



#' Sort a vector by names in ascending order
#'
#' @param x named vector
#'
#' @return
#' @export
#'
#' @examples
sort_by_name <- function(x) {
  x[order(names(x))]
  }


#' Pipe-friendly assigning of vectors when their names are known first
#'
#' @param char_vec
#' @param value
#'
#' @return
#' @export
#'
#' @examples
are_names_of <- function(char_vec, value) {
  if(length(value)==1) {value <- rep(value, length(char_vec))}
  structure(value, names = char_vec)
}


#' Load the default values of formals of a function into the Global environment
#'
#' @param ... the function
#' @param indices the indices of the formals you want to evaluate
#' @details Useful for debugging
#'
#' @return
#' @export
#'
#' @examples
#' evaluate_formals(lm, indices = 1:5)
evaluate_formals <- function(..., indices = 1:length(formals(...))) {
  indices %>% lapply(. %>% {assign(x = names(formals(...))[[.]],
                                   value = formals(...)[[.]],
                                   envir = .GlobalEnv)})
}


#' subset a vector by matching names against a pattern
#'
#' This function is inspired by str_subset from stringr
#'
#' @param vec
#' @param pattern
#'
#' @return
#' @export
#'
#' @examples
str_subset_name <- function(vec, pattern) {
  vec %>% .[str_detect(names(.), pattern)]}

#' @export
#' @rdname str_subset_name
"str_subset_name<-" <-  function(vec, pattern,value) {
  vec[str_detect(names(vec), pattern)] <- value
  vec}

#' str_subset that preserves names
#'
#' @param vec
#' @param pattern
#'
#' @return
#' @export
#'
#' @examples
str_subset_keep_names <- function(vec, pattern) {
  vec %>% .[str_detect(., pattern)]}


#' the opposite of str_subset.
#'
#' Preserves names
#'
#' @param vec
#' @param pattern
#'
#' @return
#' @export
#'
#' @examples
str_subset_not <- function(vec, pattern) {
  vec %>% .[!str_detect(.,pattern)]}


#' str_subset_name
#'
#' @param vec
#' @param pattern
#'
#' @return
#' @export
#'
#' @examples
str_subset_keep_names_not <- function(vec, pattern) {
  vec %>% .[!str_detect(., pattern)]}


#' nicely formatted dput for named vectors
#'
#' @param myvec
#'
#' @return
#' @export
#'
#' @examples
print_r.named_vector <- function(myvec) {myvec %>%
    paste(str_pad(names(.), width = max(str_length(names(.))), side = "right"), "=", ., sep = "\t") %>%
    paste0(collapse = "\t,\n") %>% paste("c(\n",.,"\n)") %>%
    cat}




#' Append a time-stamp before
#'
#' @param mystring
#'
#' @return
#' @export
#'
#' @examples
tpaste0 <- function(...) {
  paste0(format(Sys.time(), "%Y_%m_%d_%H_%M")
         , "_" , ...)
}


#' List the elements of .GlobalEnv without elements with matching naes
#'
#' @param reg A vector of regex to be matched
#'
#' @return
#' @export
#'
#' @examples
global_env_without <- function(reg) ls(.GlobalEnv)[!(ls(.GlobalEnv) %>% sapply(. %>% str_detect(reg) %>% any))]


#' The runtime of some code
#'
#' This function can be used, when you want to benchmark, but also access the results.
#' @param ... Some code
#'
#' @return The output of the code with an attribute "runtime"
#' @export
#'
#' @examples
runtime <- function( ... ) {
  pt <- proc.time()
  myenv <- environment()
  out <- eval(as.expression(substitute(...)), envir = myenv)
  pt <- proc.time()-pt
  attr(out, "runtime") <- pt
  return(out)
}
