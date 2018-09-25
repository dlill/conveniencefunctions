# install ----

# R -e 'devtools::install_github("dlill/conveniencefunctions")'

#' Write the output of the check to a file
#'
#' @param path where the output should be sunk to
#'
#' @export
check2sink <- function(path = "check.txt") {
  sink(path)
  devtools::check(args = c('--as-cran'), build_args = c('--no-build-vignettes'))
  sink()
}

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
#' @param pattern A vector of regex to be matched
#'
#' @export
global_env_without <- function(pattern) {
  ls(.GlobalEnv)[!(ls(.GlobalEnv) %>% sapply(. %>% str_detect(pattern) %>% any))]
}


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
#' @param vec the vector where the values should be inserted
#' @param values the vector with the replacements
#'
#' @return the modified vector
#' @export
#'
#' @examples
#' vec <- letters[1:3] %>% are_names_of(0)
#' vals <- letters[2:4] %>% are_names_of(1)
#' insert_values(vec, vals)
insert_values_by_name <- function(vec, values) {
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

#' quasi random seed when a seed is already set
#'
#' @export
time_to_seed <- function(){ date() %>% as.character.Date() %>% str_extract_all("[0-9]") %>% do.call(c,.) %>% paste0(collapse = "") %>% as.numeric() %>%  `/`(.,10000)  %>% round() %>% as.integer()
}


# move this to ./data/
cf_tibble <- tibble::tibble(a = 1:4, b = c(1,1,2,2), d = letters[1:4])



#' Unnesting functions for list-columns
#'
#' The following functions serve two purposes
#' 1. Keep other list-columns when unnesting one specific list-column
#' 2. Keep names of list-columns when unnesting them
#' 3. Make it possible to spread a list-column
#'
#' However, this inroduces the need for a key-column.
#'
#' @param df a tibble with list columns
#' @param key,value like in gather and spread
#'
#' @export
#'
#' @examples
#' df <- tibble(a = 1:2, b = list(c(d = 1, e = 2), c(e = 2, f = 4)), g = list(1, 2:3)) #%>% map(setNames, nm = c("c", "d")))
#'
#' df %>% unnest(b)
#'
#' df %>% spread_list_column(a,b) %>%
#'   print
#'
#' df %>% unnest_named(a,b)
unnest_named <- function(df, key, value) {

  key <- enquo(key)
  value <- enquo(value)

  key_n <- quo_name(key)
  value_n <- quo_name(value)

  myframe_1 <- select(df, - !! value)

  myframe_2 <- df %>% select(!!key, !!value)

  myframe_2 <- unnest(myframe_2, !!value)

  if(!is.null(names(df[[!!key]][[1]])))
    map()

  myframe_2 <- map(1:nrow(myframe_2), function(i) {
    df <- myframe_2[i,]

    mykey <- select(df, !!key)[[1]]
    myval <- select(df, !!value)[[1]][[1]]

    out <- tibble(!! key_n := mykey, !! value_n := myval)

    if(!is.null(names(myval))) {
      name_n <- paste0("names_", value_n)
      out <- bind_cols(out, !!name_n := names(myval))
    }

    return(out)
  }) %>% bind_rows()

  left_join(myframe_1, myframe_2, by = key_n)

}


#' @rdname unnest_named
#' @export



#' Quickly append a column which contains the rownumber
#'
#' @param df tibble or data.frame
#'
#' @return df with additional column "rownumber"
#' @export
append_rownumber_col <- function(df) {
  df %>% mutate(., rownumber = 1:nrow(.))
}



#' Send a mail informing you
#'
#' @param job_name the name of the runbg_job
#'
#' @export
#'
#' @importFrom mailR send.mail
send_runbg_mail <- function(job_name, to = "dl140@physik.uni-freiburg.de") {
  try(mailR::send.mail(from = "myRunbgJob@gmail.com",
                   to = to,
                   subject=paste0("Job  done"),
                   body = paste("The job", job_name, "from machine", Sys.info()["nodename"], "is ready to fetch :) "),
                   smtp = list(host.name = "smtp.gmail.com", port = 465,
                               user.name = "myRunbgJob",
                               passwd = digest::digest("myRunbgJob"),
                               ssl = TRUE),
                   authenticate = TRUE,
                   send = TRUE))
}

