# useful stringr like functions ----
#' Align some lines of code along a pattern
#'
#' @param string string
#' @param pattern pattern
#'
#' @return aligned string
#' @export
#'
#' @examples
#' mystr <- 'a<- 1
#' sdlfkjslkdjlskdfjlsjdkjslkd<-2'
#' str_align(mystr)
str_align <- function(string, pattern = "<-") {
  split1 <- str_split(string, "\\n", simplify = T)
  mysplit <- str_split(split1, pattern, simplify = T)
  len1 <- max(str_length(mysplit[,1]))
  aligned <- sprintf(paste0("%-", len1, "s", pattern, "%s"), mysplit[,1], mysplit[,2])
  paste(aligned, collapse = "\n")
}

#' Escape all special characters in a string
#'
#' @param string string
#'
#' @return string with single escapes before special characters (don't be confused by the double escapes from "print")
#' @export
#'
#' @examples
#' str_escape("a.(asdfsadf)") %>% cat
str_escape <- function(string) {
  message("Remember to cat() to see result with single escapes.")
  str_replace_all(string, "([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1")
}

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
  vec %>% .[stringr::str_detect(names(.), pattern)]}

#' @export
#' @rdname str_subset_name
"str_subset_name<-" <-  function(vec, pattern,value) {
  vec[stringr::str_detect(names(vec), pattern)] <- value
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
  vec %>% .[stringr::str_detect(., pattern)]}


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
  vec %>% .[!stringr::str_detect(.,pattern)]}


#' @rdname str_subset_not
str_subset_keep_names_not <- function(vec, pattern) {
  vec %>% .[!stringr::str_detect(., pattern)]}


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
  sapply(vec, . %>% stringr::str_detect(pattern) %>% any) %>% any
}


#' Title
#'
#' @param path filepath
#' @param pattern,replacement as in str_replace
#' @param walkthrough walk through each matching line and decide
#' @param writeout write to file
#'
#' @return the modified string
#' @export
str_replace_in_file <- function(path, pattern, replacement, walkthrough = F, writeout = F) {
  string <- readLines(path)
  
  strsubset <- str_subset(string, pattern)
  if (!writeout){
    cat("original---------------------------\n")
    print(strsubset)
    print("# ---------------------------------------------------------- #
    # new ----
    # ---------------------------------------------------------- #")
    print(str_replace_all(strsubset, pattern, replacement))
  }
  
  do_replace <- logical(length(strsubset))
  if (walkthrough) {
    for (i in seq_along(do_replace)) {
      do_replace[i] <- readline(paste0("replace in: ", strsubset[i]))
    }
    strsubset[do_replace] <- str_replace_all(strsubset[do_replace], pattern, replacement)
    string[str_detect(pattern)] <- strsubset
  }
  
  if(writeout)
    writeLines(string, path)
  attr(string, "do_replace") <- do_replace
  return(string)
}

