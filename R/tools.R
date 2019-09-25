
# Analysis ----

#' str with max.level = 0
#'
#' Saves you typoing when analysing objects
#' @param ... an object
#'
#' @export
str0 <- function(...) {
  str(..., max.level = 0, give.attr = F)
}

#' @rdname str0
#' @export
str1 <- function(...) {
  str(..., max.level = 1, give.attr = F)
}

#' @rdname str0
#' @export
str2 <- function(...) {
  str(..., max.level = 2, give.attr = F)
}

#' @rdname str0
#' @export
stra0 <- function(...) {
  str(..., max.level = 0, give.attr = T)
}

#' @rdname str0
#' @export
sttr0 <- function(...) {
  str(..., max.level = 0, give.attr = T)
}

#' @rdname str0
#' @export
strr0 <- function(...) {
  str(..., max.level = 0, give.attr = T)
}

#' @rdname str0
#' @export
stra1 <- function(...) {
  str(..., max.level = 1, give.attr = T)
}

#' @rdname str0
#' @export
sttr1 <- function(...) {
  str(..., max.level = 1, give.attr = T)
}

#' @rdname str0
#' @export
strr1 <- function(...) {
  str(..., max.level = 1, give.attr = T)
}

stra2 <- function(...) {
  str(..., max.level = 2, give.attr = T)
}

#' @rdname str0
#' @export
sttr2 <- function(...) {
  str(..., max.level = 2, give.attr = T)
}

#' @rdname str0
#' @export
strr2 <- function(...) {
  str(..., max.level = 2, give.attr = T)
}

#' @rdname str0
#' @export
head1 <- function(...) {
  head(..., n = 1)
}

#' @rdname str0
#' @export
head3 <- function(...) {
  head(..., n = 3)
}

#' @rdname str0
#' @export
head11 <- function(...) {
  x <- list(...)
  x[[1]][[1]] %>% head(n = 5)
}

#' @rdname str0
#' @export
tail1 <- function(...) {
  tail(..., n = 1)
}

#' @rdname str0
#' @export
tail3 <- function(...) {
  tail(..., n = 3)
}

# Useful vector operations ----

#' Subtract elements with matching names
#'
#' @param .x,.y vecs or matrices
#'
#' @return .y-.x at the respective positions
#' @export
subtract_by_name <- function(.x,.y) {
  
  if(length(dim(.x))!=length(dim(.y)))
    stop("not the same number of dimensions")
  if (is.null(dim(.x))) {
    if(!identical(order(names(.x)), order(names(.y))))
      warning("Names not in identical order")
    return(.y[intersect(names(.x), names(.y))] - .x[intersect(names(.x), names(.y))])
  }
  if (length(dim(.x)) == 2) {
    if(!(identical(order(rownames(.x)), order(rownames(.y)))&identical(order(colnames(.x)), order(colnames(.y)))))
      warning("Dimnames not in identical order")
    return(  .y[intersect(dimnames(.x)[[1]], dimnames(.y)[[1]]),
                intersect(dimnames(.x)[[2]], dimnames(.y)[[2]])] -
               .x[intersect(dimnames(.x)[[1]], dimnames(.y)[[1]]),
                  intersect(dimnames(.x)[[2]], dimnames(.y)[[2]])])
  }
}


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
#' @param vec the vector where the values should be inserted
#' @param values the vector with the replacements
#'
#' @return the modified vector
#' @export
#'
#' @examples
#' vec <- letters[1:3] %>% are_names_of(0)
#' vals <- letters[2:4] %>% are_names_of(1)
#' insert_values_by_name(vec, vals)
insert_values_by_name <- function(vec, values) {
  oldnames <- names(vec)
  vec <- sort_by_name(vec)
  values <- sort_by_name(values)
  mynames <- intersect(names(vec), names(values))
  vec[mynames] <- values[mynames]
  return(vec[oldnames])
}



# Other useful stuff ----


#' Write a regex to search for all function names in a package
#'
#' @param package string of length 1
#' @param as_namespace print everything in the namespace or just exported objects?
#'
#' @return a fancy regex
#' @export
#'
#' @examples
#' funnames_in_package("conveniencefunctions")
funnames_in_package <- function(package, as_namespace = F) {
  wup <- ls(paste0("package:", package))
  if (as_namespace)
    wup <- ls(envir = asNamespace(package))
  wup %>%
    str_escape %>%
    paste(collapse = "|") %>%
    paste0("\\b(", ., ")\\b")
}



