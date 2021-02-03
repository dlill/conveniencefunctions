#' Flexzirkel Stoppuhr
#'
#' "coin" = trainieren, "work complete" = aufhö?ren, "bing" = gleich gehts wieder los
#'
#' @export
#' @importFrom beepr beep
flexZirkel <- function(t1 = 15,t2 = 10,t3 = 10,t4 = 7,t5 = 5) {
  cat("Tipps",
      "* Bei Uebung 1 die Beine durchstrecken",
      "* Bei Uebung 2 den oberen Arm langziehen",
      "* Bei Rollbrett nach vorne Beine durchstrecken",
      "Uebungen",
      "* Fussverschraubung",
      "* Pinguin",
      "* Krebs",
      "* Muschel",
      "* Kniebeugen",
      "* KlimmzUege",
      "* Sit-ups",
      "* LiegestUetze",
      "* Gymnastikball",
      sep = "\n")
  
  print(Sys.time())
  on.exit(print(Sys.time()))
  for (i in 1:100) {
    # train
    if (t1) {beepr::beep(2)
      Sys.sleep(t1)}
    if (t2) {beepr::beep(2)
      Sys.sleep(t2)}
    if (t3) {beepr::beep(2)
      Sys.sleep(t3)}
    beepr::beep(4)
    # change
    Sys.sleep(t4)
    beepr::beep(1)
    Sys.sleep(t5)
  }
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


#' Turn correlation matrix and standard deviations back to covariance matrix
#'
#' @param mycor correlation matrix
#' @param mysd standard deviations
#'
#' @return covariance matrix
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
#'
#' @examples
#' mycor <- matrix(c(1,0.8,0.8,1),2)
#' mysd <- c(sqrt(2),1)
#' cor2cov(mycor,mysd)
cor2cov <- function(mycor,mysd = rep(1,dim(mycor)[1])) {
  sdmat <- matrix(mysd, nrow = length(mysd), ncol = length(mysd))
  sdmat <- sdmat * t(sdmat)
  mycov <- mycor * sdmat
}



# Other useful stuff ----

#' Run expression and print OK/FAILED if expression returns TRUE/FALSE
#' 
#' Daniel Kaschek's check function
#' @param message character, e.g., "if all subjects are in data".
#' @param expr the expression to be evaluated
#' @param nchar integer, maximum number of character for message (for nicer print-out)
#' @export 
check <- function(message, expr, nchar = 80, FLAGthrowError = FALSE) {
  
  dots <- paste(rep(".", nchar), collapse = "")
  message <- paste(crayon::bold("Checking"), message)
  message <- paste(message, dots)
  message <- substr(message, 1, nchar)
  
  cat(message)
  check_passed <- eval(expr)
  
  if (check_passed) 
    cat(crayon::green(" OK\n"))
  else
    cat(crayon::red(" FAILED\n"))
  if (!check_passed & FLAGthrowError) 
    stop("Check failed: ", message)
  
  invisible()
}


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

# -------------------------------------------------------------------------#
# File interactions ----
# -------------------------------------------------------------------------#

#' gitall from R command line
#'
#' @param string commit message
#'
#' @return
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
gall <- function(string) {
  system(paste0('git add --all && 
  git commit -m "', string, '" && 
  git pull && 
  git push'), wait = FALSE)
}

#' gitcom from R command line
#'
#' @param string commit message
#'
#' @return
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
gitcom <- function(string) {
  system(paste0('git add --all && 
  git commit -m "', string, '"'), wait = FALSE)
}

#' gitcom --amend from R command line
#'
#' @param string commit message
#'
#' @return
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
gitam <- function(string) {
  if (missing(string)){
    cmd <- 'git add --all && git commit --amend --no-edit'
  } else {
    cmd <- paste0('git add --all && git commit --amend -m "', string, '"')
  }
  system(cmd, wait = FALSE)
}


#' Git pull all projects in the PROJTOOLS fodler
#'
#' @export
#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
#'
#' @examples
pullJobs <- function() {
  setwd("~")
  f <- c(list.files("PROJTOOLS/", full.names = TRUE),
         list.files("PROJECTS/", full.names = TRUE))
  system(paste0(paste0("cd && cd ",f, " && git pull && echo ", f), collapse = "&"))
}



#' Write a characeter vector to a file and open it.
#'
#' @param .x vector
#'
#' @export
open_in_file <- function(.x) {
  tf <- tempfile()
  .x%>% writeLines(tf) %>% file.edit(tf)
}

#' Write an excel file and cat a name to open it.
#'
#' @param .x data.frame
#'
#' @export
open_in_calc <- function(.x) {
  tf <- tempfile()
  tf <- paste0(tf, ".csv")
  write_csv(.x, tf)
  paste0("libreoffice --calc ", tf) %>% cat
}

#' @export
tpaste0 <- function(...) {paste0(format(Sys.time(), "%y%d%m_%H%M%S-"), ...)}

# -------------------------------------------------------------------------#
# Install ----
# -------------------------------------------------------------------------#
#' @export
cf_update <- function() {
  devtools::install_github("dkaschek/cOde", dependencies = FALSE)
  devtools::install_github("dkaschek/dMod", dependencies = FALSE)
  devtools::install_github("dlill/conveniencefunctions", dependencies = FALSE)
}






# -------------------------------------------------------------------------#
# Password generator ----
# -------------------------------------------------------------------------#

afthe_except_man_been_those_ipsum_to_and_is_Lorem_cupiditate_theo_Ireprehenderit_always_accident_ex_quo_scrambled_nihil_praesent <- function(N = 30, seed = Sys.time()) {
  digest::digest(seed) %>% stringr::str_extract_all("\\d", T) %>% .[1:8] %>% paste0(collapse = "") %>% as.numeric() %>% set.seed
  (38:(38+88)) %>% as.raw() %>% imap_chr(rawToChar) %>% sample(N, TRUE) %>% paste0(collapse = "")
}

afthe_except_man_been_those_ipsum_to_and_is_Lorem_cupiditate_theo_Ireprehenderit_always_accident_ex_quo_scrambled_nihil_praesent2 <- function(N = 5, seed = Sys.time()) {
  wl <- read.table("inst/effWordlist/eff_large_wordlist.txt", sep = " ", row.names = NULL,stringsAsFactors = FALSE) 
  
  digest::digest(seed) %>% stringr::str_extract_all("\\d", T) %>% .[1:8] %>% paste0(collapse = "") %>% as.numeric() %>% set.seed
  sample(wl$V3, N) %>% paste0(collapse = "")
}














