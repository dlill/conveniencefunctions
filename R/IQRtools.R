#' Rename a script, change filename occurences within script, rename output folder
#'
#' @param from,to as in file.rename 
#'
#' @export
cf_rename_script <- function(from, to) {
  cf_copy_script(from, to, TRUE)
}



#' Rename a script, change filename occurences within script, rename output folder
#'
#' @param from,to as in file.rename 
#'
#' @export
cf_copy_script <- function(from, to, FLAGremoveOld = FALSE) {
  from_stripped <- stringr::str_replace_all(from, c("\\.R$" = "", "^SCRIPT_" = ""))
  to_stripped   <- stringr::str_replace_all(to,   c("\\.R$" = "", "^SCRIPT_" = ""))
  
  ln <- readLines(from)
  message("Number of replaced filename references: ", sum(stringr::str_count(ln, from_stripped)), "-------\n")
  ln <- stringr::str_replace_all(ln, from_stripped, to_stripped)
  
  writeLines(ln, to)
  
  if (FLAGremoveOld) unlink(from)
  
  inspire()
}


#' Get some inspiration
#'
#' @return
#' @export
#'
#' @examples
#' inspire()
inspire <- function() {
  cat("Starting a new analysis? Here are some strategies", 
      "* Talk",
      "  * The more one talks in the beginng of a project, the less work it is in the end",
      "  * Take time for an in-depth discussion of the data (see 'overview'), pitfalls etc...",
      "* Complexity, Divide and Conquer",
      "  * Acknowledge a problem's complexity and its difficulties",
      "  * Don't try to solve everything at once",
      "  * Think of representative examples",
      "  * First look at the factors of the data and search for sensible lines to divide",
      "* Overview",
      "  * Create a data.table 'ID' containing all factors of the data and think of additional columns", 
      "  * Re-program some existing columns like TRTNAME to ensure consistent formatting.", 
      "  * Generate overview tables: How many records here when summarized by this and that factor...",
      "* Abstraction", 
      "  * Don't tune hyperparameters of an algorithm too often without abstracting the problem",
      "    * Try to understand the hyperparameters but realize when you're wasting time",
      "* Coding",
      "  * Strictly separate data programming from modeling. If you develop a modeling script along with data pgroamming, that's fine, but the script should be split afterwards.",
      "  * Don't handle more than 3 data.tables at once. If more, go back and think, is it necessary?",
      "* Results",
      "  * Produce quality plots from the start and explain them, IQslides with plot and IQbulletlist might help.",
      "  * Results are single, specific statements. They are not endless pdf files full of plots.",
      "  * Produce communicatable results",
      "* Copy these tips into the script to not forget them :)",
      sep = "\n")
}
