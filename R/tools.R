# install ----

# R -e 'devtools::install_github("dlill/conveniencefunctions")'


# Analysis ----

#' str with max.level = 0
#'
#' @param ...
#'
#'
#' @export
#'
#'
str0 <- function(...) {
  str(..., max.level = 0)
}

#' str with max.level = 1
#'
#' @param ...
#'
#'
#' @export
#'
#'
str1 <- function(...) {
  str(..., max.level = 1)
}

#' str with max.level = 2
#'
#' @param ...
#'
#'
#' @export
#'
#'
str2 <- function(...) {
  str(..., max.level = 2)
}



#' Load the default values of formals of a function into the Global environment
#'
#' @param ... the function
#' @param indices the indices of the formals you want to evaluate
#' @details Useful for debugging
#'
#'
#' @export
#'
#' @examples
#' evaluate_formals(lm, indices = 1:5)
evaluate_formals <- function(..., indices = 1:length(formals(...))) {
  indices %>% lapply(. %>% {assign(x = names(formals(...))[[.]],
                                   value = formals(...)[[.]],
                                   envir = .GlobalEnv)})
}



#' List the elements of .GlobalEnv without elements with matching naes
#'
#' @param reg A vector of regex to be matched
#'
#' @export
global_env_without <- function(reg) ls(.GlobalEnv)[!(ls(.GlobalEnv) %>% sapply(. %>% str_detect(reg) %>% any))]


#' The runtime of some code
#'
#' This function can be used, when you want to benchmark, but also access the results.
#' @param ... Some code
#'
#' The output of the code with an attribute "runtime"
#' @export
#'
#' @examples
#' runtime({
#' paste0(1:260000, letters, LETTERS)
#' })
runtime <- function( ... ) {
  pt <- proc.time()
  myenv <- environment()
  out <- eval(as.expression(substitute(...)), envir = myenv)
  pt <- proc.time()-pt
  attr(out, "runtime") <- pt
  return(out)
}




# Useful vector operations ----

#' Sort a vector by names in ascending order
#'
#' @param x named vector
#'
#' @export
#'
#' @examples
#' c(b = 1, a = 2) %>% sort_by_name
sort_by_name <- function(x) {
  x[order(names(x))]
  }


#' Pipe-friendly assigning of vectors when their names are known first
#'
#' @param char_vec Character
#' @param value Thre possibilities: 1. A function which takes \code{n} as an argument such as \code{rnorm}.
#' 2. A vector of length 1, then this value gets recycled.
#' 3. a vector of length \code{length(char_vec)}
#' @param ... Arguments ging to value if value is a function
#'
#' @export
#'
#' @examples
#' letters %>% are_names_of(rnorm)
#' letters %>% are_names_of(1)
#' letters %>% are_names_of(1:26)
are_names_of <- function(char_vec, value, ...) {

  if(is.function(value)) value <- do.call(value, list(n = 1:length(char_vec), ...))
  else if(length(value)==1) value <- rep(value, length(char_vec))

  structure(value, names = char_vec)
}

#' Insert values from another vector with some shared names
#'
#' @param vec
#' @param values
#'
#' @return the modified vector
#' @export
#'
#' @examples
#' vec <- letters[1:3] %>% are_names_of(0)
#' vals <- letters[2:4] %>% are_names_of(1)
#' insert_values(vec, vals)
insert_values <- function(vec, values) {
  oldnames <- names(vec)
  vec <- sort_by_name(vec)
  values <- sort_by_name(values)
  mynames <- intersect(names(vec), names(values))
  vec[mynames] <- values[mynames]
  return(vec[oldnames])
}




# useful stringr like functions ----



#' subset a vector by matching names against a pattern
#'
#' This function is inspired by str_subset from stringr
#'
#' @param vec named vector
#' @param pattern regex as in \link{str_subset}
#'
#' @export
#'
#' @examples
#' myvec <- c("a", "b", "ab", "ba") %>% are_names_of(runif)
#' myvec %>% str_subset_name("^a")
str_subset_name <- function(vec, pattern) {
  vec %>% .[str_detect(names(.), pattern)]}

#' @export
#' @rdname str_subset_name
"str_subset_name<-" <-  function(vec, pattern,value) {
  vec[str_detect(names(vec), pattern)] <- value
  vec}

#' str_subset that preserves names
#'
#' @param vec named vector
#' @param pattern regex as in str_subset
#'
#' @export
#'
#' @examples
#' myvec <- c("a", "b", "ab", "ba") %>% are_names_of(.,.)
#' myvec %>% str_subset_keep_names("^a")
str_subset_keep_names <- function(vec, pattern) {
  vec %>% .[str_detect(., pattern)]}


#' The "opposite" of str_subset.
#'
#' Preserves names
#'
#' @param vec  named vector
#' @param pattern regex as in str_subset
#'
#' @export
#'
#'
str_subset_not <- function(vec, pattern) {
  vec %>% .[!str_detect(.,pattern)]}


#' @rdname str_subset_not
str_subset_keep_names_not <- function(vec, pattern) {
  vec %>% .[!str_detect(., pattern)]}


#' Is (any element) of pattern in any of the elements of vec?
#'
#' I usually use it with a pattern of length 1.
#'
#' @param vec named vector
#' @param pattern regex as in str_subset
#'
#' Logical
#' @export
#'
#' @examples
#' str_detect_any(letters[1:3], c("c"))
#' str_detect_any(letters[1:3], c("d"))
#' str_detect_any(letters[1:3], c("c","d"))
str_detect_any <- function(vec, pattern) {
  sapply(vec, . %>% str_detect(pattern) %>% any) %>% any
}

str_detect_any(letters[1:3], c("c","d"))




# Other useful stuff ----


#' Append a time-stamp before a string
#'
#' @param mystring
#'
#' @export
#'
#' @examples
#' tpaste0("workspace.rda")
tpaste0 <- function(...) {
  paste0(format(Sys.time(), "%Y_%m_%d_%H_%M")
         , "_" , ...)
}




# move this to ./data/
cf_tibble <- tibble::tibble(a = 1:4, b = c(1,1,2,2), d = letters[1:4])



#' Unnest a list-column into a key-value pair, if the list entries are named vectors
#'
#' This function was built to look at the results of obj_condition_wise more easily
#'
#' @param myframe a tibble with list columns
#' @param unique Character. A column name with unique elements in each row. \code{myframe} is split up and joined back together,
#' so a unique identifier is needed to merge the two tibbles back together.
#' @param unnest_var Character. The column name of the column which should be spread
#'
#'  @export
#'
#' @examples
#' tibble(condition = 1, gradient = list(c(a=1, b=2, c=3))) %>% spread_list_column()
spread_list_column <- function(myframe, unique = "condition", unnest_var = "gradient") {
  myframe_2 <- myframe %>% .[c(unique, unnest_var)]
  myframe_2 <- myframe_2 %>% apply(1, function(i) {
    tibble(condition = i[[unique]], grad_value = unlist(i[[unnest_var]]), grad_name = unlist(names(i[[unnest_var]])))
  }) %>% do.call(rbind,.)

  left_join(myframe, myframe_2)

}

#' @rdname spread_list_column
unnest_name <- spread_list_column

