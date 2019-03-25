# install ----

# R -e 'devtools::install_github("dlill/conveniencefunctions", upgrade_dependencies = F)'

#' Title
#'
#' @return
#' @export
#'
#' @examples
load_cf <- function() {
  devtools::load_all("~/Promotion/Projects/conveniencefunctions/")
}

#' @export
#' @rdname load_cf
load_dMod <- function() {
  devtools::load_all("~/Promotion/Software/dMod/")
}

#' Open all scripts of a folder in rstudio
#' @export
open_all_scripts_in_dir <- function(dirname = "Scripts", pattern = "\\.R$"){
  myorder <- list.files(dirname, pattern, full.names = TRUE) %>% str_replace_all(".*S([0-9]+).*", "\\1") %>% as.integer() %>% order
  list.files(dirname, pattern, full.names = TRUE) %>% .[myorder] %>% walk(~rstudioapi::navigateToFile(.x))
}



# check2sink ----

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

# diff ----

#' Compare two strings in meld
#'
#' @param string1,string2,string3 strings to compare
#' @param filenames dummy filenames
#' @param do_unlink remove the files
#'
#' @return side-effect
#' @export
#'
#' @family meld-functions
#' @examples
#' meld_functions(lm, glm)
meld_strings <- function(string1, string2, string3 = NULL, filenames = "dummymeldcomparison", do_unlink = T) {
  writeLines(string1, paste0(filenames, "1.txt"))
  writeLines(string2, paste0(filenames, "2.txt"))
  if (!is.null(string3))
    writeLines(string3, paste0(filenames, "3.txt"))
  filenumbers <- switch(is.null(string3), "TRUE" = 1:2, "FALSE" = 1:3)
  system2("meld", paste0(filenames, filenumbers, ".txt"), wait = F)
  if (do_unlink) {
    Sys.sleep(2)
    unlink(paste0(filenames, filenumbers, ".txt"))
  }
}

#' meld-diff two functions
#'
#' @param function1,function2
#'
#' @return side-effect
#' @export
#'
#' @family meld-functions
#' @examples
#' meld_functions(lm, glm)
meld_functions <- function(function1, function2, filenames = "dummymeldcomparison", do_unlink = T, do_trim = F) {
  string1 <- capture.output(print(function1))
  string2 <- capture.output(print(function2))
  if (do_trim) {
    string1 <- str_trim(string1)
    string2 <- str_trim(string2)
  }
  meld_strings(string1, string2, filenames = filenames ,do_unlink = do_unlink)
}


#' meld 2 folders
#'
#' @param folder1,folder2  paths
#' @export
#'
meld_folders <- function(folder1, folder2) {
  system2("meld", c(folder1, folder2))
}


# system/file interactions ----

#' Relative path between two absolute paths
#'
#' @param from,to absPaths
#'
#' @return relPath
#' @export
#'
#' @examples
#' relPath_from_1_to_2("~/test", "/usr/local")
relPath_from_1_to_2 <- function(from, to) {
  from <- path.expand(from)
  to <- path.expand(to)

  split1 <- str_split(from, "/", simplify = T) %>% `dim<-`(NULL)
  split2 <- str_split(to, "/", simplify = T) %>% `dim<-`(NULL)

  minlen <- min(c(length(split1), length(split2)))
  root <- map2_lgl(split1[1:minlen], split2[1:minlen], ~.x == .y) %>% cumsum %>% max

  backwards <- rep("../", length(split1) - root) %>% paste0(collapse = "")
  forwards <- split2[-c(1:root)] %>% paste0("/") %>% paste0(collapse = "")

  return(paste0(backwards, forwards))
}

#' remove the document name from a document path
#'
#' @param doc can either be  rstudioapi::getActiveDocumentContext() or a path
#'
#' @return the path with the document removed
#'
#' @importFrom stringr str_split
#'
#' @export
docpath2dirpath <- function(doc) {
  if(class(doc) == "document_context")
    doc <- doc$path
  doc %>% stringr::str_split("/", simplify = T) %>% .[1:(length(.)-1)] %>% paste0(collapse = "/") %>% paste0("/")
}


#' Set working directory to this document's directory
#'
#' @export
#'
thisdocsetwd <- function() {
  rstudioapi::getActiveDocumentContext() %>% docpath2dirpath() %>% setwd()
}


#' Sequential file numbering
#'
#' Create filenames myfile001.png myfile002.png ... automatically
#'
#' @param basename,fileext,filepath,addition strings that are concatenated like this: paste0(filepath, basename, NUMBER, addition, fileext)
#'
#' @author dww from stackoverflow https://stackoverflow.com/questions/54752246/automatic-file-numbering-in-ggsave-as-in-png
#' @return character
#' @export
#'
#' @examples
#' nextfile()
next_file <-  function(addition = "", base_name = '', fileext = 'png', filepath = '.', purge = FALSE){

  if (exists(".base_name", .GlobalEnv) & str_length(base_name) == 0){
    base_name <- .base_name
  }

  if (purge) {
    files <- list.files(filepath, paste0(base_name,'\\d+.*\\.', fileext,'$'))
    unlink(file.path(filepath, files))
    return(TRUE)
  }


  old.fnames = grep(paste0(base_name,'\\d+.*\\.', fileext,'$'),
                    list.files(filepath), value = T)
  lastnum = gsub(paste0(base_name,'(\\d+).*\\.', fileext,'$'), '\\1', old.fnames)
  if (!length(lastnum)) {
    lastnum = 1
  } else {
    lastnum = sort(as.integer(lastnum),T)[1] + 1L
  }
  return(paste0(base_name, sprintf('%03i', lastnum), addition, '.', fileext))
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
#' @importFrom stringr str_detect
#'
#' @export
global_env_without <- function(pattern) {
  ls(.GlobalEnv)[!(ls(.GlobalEnv) %>% sapply(. %>% stringr::str_detect(pattern) %>% any))]
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


#' Clear the warnings-list
#'
#' @export
#'
#' @examples
#' warning("noooo")
#' warnings()
#' flush_warnings()
#' warnings()
flush_warnings <- function() {
  assign("last.warning", NULL, envir = baseenv())
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
#' @importFrom stringr str_extract_all
#' @export
time_to_seed <- function(){ date() %>% as.character.Date() %>% stringr::str_extract_all("[0-9]") %>% do.call(c,.) %>% paste0(collapse = "") %>% as.numeric() %>%  `/`(.,10000)  %>% round() %>% as.integer()
}



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


