
# yac_simplify <- function(string, FLAGsimplify = TRUE, FLAGverbose = FALSE) {
#   map_chr(string, function(.x) {
#     
#     if (FLAGverbose)
#       print(.x)
#     
#     symbols_old <- getSymbols(.x)
#     symbols_new <- str_replace_all(symbols_old, "_", "XXXXXX")
#     .y <- replaceSymbols(symbols_old, symbols_new, .x)
#     .y <- yacas(.y)
#     if (FLAGsimplify)
#       .y <- Simplify(.y)
#     .y <- as.character(.y)
#     replaceSymbols(symbols_new, symbols_old, .y)
#   })
# }


#' Simplify with Mathematica
#' 
#' Escapes _ and puts everything into yacas/Mathematica for simplification
#' 
#' yac_simplify (currently disabled) needs yacas and Ryacas installed. mat_simplify needs wolframscript and Matrhematica installed
#' 
#' @param string named vector with equations, not too complicated please. # [] For Mathematica, replacements of sin(x) -> Sin[x] still need to be done
#' @param FLAGsimplify call yacas::Simplify or rely solely on the conversion string -> yacas -> string for the simplification
#' @param FLAGverbose print input
#' 
#' @return named vector with simplified equations
#' 
#' @export
#' @importFrom cOde getSymbols replaceSymbols
#' @importFrom stringr str_replace_all str_split
#' @importFrom purrr map_chr
mat_simplify <- function(string) {
  symbols_old <- cOde::getSymbols(string)
  symbols_new <- stringr::str_replace_all(symbols_old, "_", "XXXXXX")
  .y <- map_chr(string, ~cOde::replaceSymbols(symbols_old, symbols_new, .x))
  cmd <- paste0("wolframscript -code Simplify[", paste0("{", paste0(.y, collapse = ","), "}") , "]")
  cmd <- stringr::str_replace_all(cmd, "([()])", "\\\\\\1")
  .y <- system(cmd, intern = TRUE)
  .y <- .y %>% stringr::str_replace_all("[{}]", "") %>% stringr::str_split(",", simplify = TRUE)
  .y <- purrr::map_chr(.y, ~ replaceSymbols(symbols_new, symbols_old, .x))
  .y <- setNames(.y, names(string))
}

#' @rdname mat_simplify
#' @export
#' @importFrom cOde getSymbols replaceSymbols
#' @importFrom stringr str_replace_all str_split
#' @importFrom purrr map_chr
mat_denom <- function(string) {
  symbols_old <- cOde::getSymbols(string)
  symbols_new <- stringr::str_replace_all(symbols_old, "_", "XXXXXX")
  .y <- purrr::map_chr(string, ~cOde::replaceSymbols(symbols_old, symbols_new, .x))
  cmd <- paste0("wolframscript -code Denominator[", paste0("{", paste0(.y, collapse = ","), "}") , "]")
  cmd <- stringr::str_replace_all(cmd, "([()])", "\\\\\\1")
  .y <- system(cmd, intern = TRUE)
  .y <- .y %>% stringr::str_replace_all("[{}]", "") %>% stringr::str_split(",", simplify = TRUE)
  .y <- purrr::map_chr(.y, ~ cOde::replaceSymbols(symbols_new, symbols_old, .x))
  .y <- setNames(.y, names(string))
}

#' @rdname mat_simplify
#' @export
#' @importFrom cOde getSymbols replaceSymbols
#' @importFrom stringr str_replace_all str_split
#' @importFrom purrr map_chr
mat_numer <- function(string) {
  symbols_old <- cOde::getSymbols(string)
  symbols_new <- stringr::str_replace_all(symbols_old, "_", "XXXXXX")
  .y <- map_chr(string, ~cOde::replaceSymbols(symbols_old, symbols_new, .x))
  cmd <- paste0("wolframscript -code Numerator[", paste0("{", paste0(.y, collapse = ","), "}") , "]")
  cmd <- stringr::str_replace_all(cmd, "([()])", "\\\\\\1")
  .y <- system(cmd, intern = TRUE)
  .y <- .y %>% stringr::str_replace_all("[{}]", "") %>% stringr::str_split(",", simplify = TRUE)
  .y <- purrr::map_chr(.y, ~ cOde::replaceSymbols(symbols_new, symbols_old, .x))
  .y <- setNames(.y, names(string))
}

#' @rdname mat_simplify
#' @export
#' 
#' @importFrom cOde getSymbols replaceSymbols
#' @importFrom stringr str_replace_all str_split
#' @importFrom purrr map_chr imap_chr
mat_replace <- function(string, replacements) {
  symbols_old <- cOde::getSymbols(string)
  symbols_new <- stringr::str_replace_all(symbols_old, "_", "XXXXXX")
  .y <- purrr::map_chr(string, ~cOde::replaceSymbols(symbols_old, symbols_new, .x))
  
  repl_symbols_old <- cOde::getSymbols(c(replacements, names(replacements)))
  repl_symbols_new <- stringr::str_replace_all(repl_symbols_old, "_", "XXXXXX")
  .r <- purrr::map_chr(replacements, ~cOde::replaceSymbols(repl_symbols_old, repl_symbols_new, .x))
  names(.r) <- purrr::map_chr(names(replacements), ~cOde::replaceSymbols(repl_symbols_old, repl_symbols_new, .x))
  .r <- purrr::imap_chr(.r, ~paste0("/.", .y, "->", .x))
  .r <- paste0(.r, collapse = "")
  
  cmd <- paste0("Simplify[{", paste0(.y, collapse = ","), "}", .r, "] // Print")
  tf <- tempfile()
  cmd %>% writeLines(tf)
  cmd <- paste0("wolframscript -file ", tf)
  
  .y <- system(cmd, intern = TRUE)
  .y <- .y %>% stringr::str_replace_all("[{}]", "") %>% stringr::str_split(",", simplify = TRUE)
  .y <- purrr::map_chr(.y, ~ cOde::replaceSymbols(symbols_new, symbols_old, .x))
  .y <- setNames(.y, names(string))
}



