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
cf_copy_script <- function(from, to, FLAGrename = FALSE) {
  from_stripped <- stringr::str_replace_all(from, c("\\.R$" = "", "^SCRIPT_" = ""))
  to_stripped   <- stringr::str_replace_all(to,   c("\\.R$" = "", "^SCRIPT_" = ""))
  
  ln <- readLines(from)
  message("Number of replaced filename references: ", sum(stringr::str_count(ln, from_stripped)), "-------")
  ln <- stringr::str_replace_all(ln, from_stripped, to_stripped)
  
  idx_date <- which(ln == "# [Date]") + 1
  ln[idx_date] <- paste0("# ", date())
  message("Date was updated")
  
  writeLines(ln, to)
  
  if (FLAGrename) {
    cat("Removed old file \n")
    unlink(from)
    
    cat("Rename output folder \n")
    if (from != basename(from)) stop("Renaming output folder only works if getwd() = scriptdir")
    fromOut <- file.path("../04-Output", gsub(".R$", "", from))
    toOut <- file.path("../04-Output", gsub(".R$", "", to))
    file.rename(fromOut, toOut)
    
    cat("Rename file in other scripts\n")
    cat("===============================")
    from_noEnding <- gsub("\\.R$","", basename(from))
    to_noEnding   <- gsub("\\.R$","", basename(to))
    
    allscripts <- list.files(".", "\\.R$")
    allscripts <- grep("SXXX-Rename", allscripts, value = T, invert = T)
    sapply(setNames(nm = allscripts), function(s) {
      l <- readLines(s)
      nref <-   sum(grepl(from_noEnding,l))
      if(nref==0) return(NULL)
      cat(s, ":\t", nref, "\n")
      l <- gsub(from_noEnding, to_noEnding, l)
      writeLines(l,s)
    })    
    
  }
  
  inspire()
}


#' cf_copy_script for multiple scripts at once
#'
#' @param filenames data.table(from,to)
#'
#' @return nothing, called for side-effect
#' @export
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#'
#' @examples
#' data.table(`~from` = list.files(".", "S501"),`~to` = list.files(".", "S501")) %>% cfoutput_MdTable(NFLAGtribble = 2)
#' filenames <- data.table(tibble::tribble(
#' ~from, ~to,
#' "S520-MLPSR-01-DataPreparation.R"     ,"S530-MLCLS-01-DataPreparation.R"     ,
#' "S520-MLPSR-02-ModelAgnostic.R"       ,"S530-MLCLS-02-ModelAgnostic.R"       ,
#' "S520-MLPSR-03-FitDataSetVariations.R","S530-MLCLS-03-FitDataSetVariations.R",
#' "S520-MLPSR-04-CollectModelEnsemble.R","S530-MLCLS-04-CollectModelEnsemble.R",
#' "S520-MLPSR-06-ExportTopPredictors.R" ,"S530-MLCLS-06-ExportTopPredictors.R" ,
#' ))
cf_copy_scripts_multiple <- function(filenames, FLAGrename = FALSE) {
  # copy scripts
  for (i in 1:nrow(filenames))
    conveniencefunctions::cf_copy_script(from = filenames[i,from],
                                         to = filenames[i,to], FLAGrename = FLAGrename)
  
  # replace "from" by "to" in all copied scripts
  for (i in 1:nrow(filenames)) {
    from <- basename(filenames$from[[i]])
    to   <- basename(filenames$to[[i]])
    from_noEnding <- gsub("\\.R$","", basename(filenames$from[[i]]))
    to_noEnding   <- gsub("\\.R$","", basename(filenames$to[[i]]))
    
    allscripts <- filenames$to
    sapply(setNames(nm = allscripts), function(s) {
      l <- readLines(s)
      nref <-   sum(grepl(from_noEnding,l))
      if(nref==0) return(NULL)
      cat(s, ":\t", nref, "\n")
      l <- gsub(from_noEnding, to_noEnding, l)
      writeLines(l,s)
    })
  }
  #
  invisible()
}




#' Generate filenames
#'
#' @param pattern_from 
#' @param pattern_to 
#'
#' @return data.table to use in cf_copy_script_multiple
#' @export
#'
#' @examples
#' pattern_from <- "S240-0(\\d)-Viktor-Smads"
#' pattern_to <- "S242-0\\1-SmadsRec-linearTot"
cf_script_makeFilenames <- function(pattern_from, pattern_to) {
  filenames <- data.table(from = list.files(pattern = pattern_from))
  filenames[,`:=`(to = gsub(pattern_from, pattern_to, from)), by = 1:nrow(filenames)]
  filenames <- copy(filenames) # so that it is printed directly
  filenames
}



#' Display template versions
#'
#' @param NfromLast Number of latest template versions to display
#' @param path as in list.files
#'
#' @return
#' @export
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#'
#' @importFrom data.table rbindlist setcolorder setkey
#'
#' @examples
#' path <- "/home/daniel/Promotion/Promotion/Projects/LiSyM/TGFb/Work/01-DynamicModeling/02-Scripts"
#' NfromLast = 3
#' FLAGshortenIdentical = TRUE
cf_script_templateVersions <- function(NfromLast = 3, path = ".", FLAGshortenIdentical = TRUE) {
  allscripts <- list.files(path, "\\.R$", full.names = TRUE)
  allscripts <- grep("SXXX-Rename", allscripts, value = T, invert = T)
  ti <- lapply(setNames(allscripts, basename(allscripts)), function(s) {
    l <- readLines(s)
    tn <- grep("Template name",l,value = TRUE)
    tn <- gsub("# ..( \\d)? Template name | ----- *", "", tn)
    tv <- grep("Template version",l,value = TRUE)
    tv <- gsub("# ..( \\d)? Template version | ----- *", "", tv)
    tc <- grep("Template comment",l,value = TRUE)
      tc <- gsub("# ..( \\d)? Template comment | ----- *", "", tc)
      tc <- paste0(tc, collapse = ", ") 
    data.table(templateName = tn, templateVersion = tv, templateComment = tc)
  })
  ti <- data.table::rbindlist(ti, idcol = "script")
  ti <- ti[!is.na(templateName)]
  ti <- ti[order(templateName, templateVersion)]
  
  # Remove old templates
  ti[,`:=`(tvnum = as.numeric(as.factor(templateVersion)))]
  ti[,`:=`(keep = tvnum > max(tvnum)-NfromLast), by = "templateName"]
  ti <- ti[keep == TRUE]
  
  # collapse reused templates in different scripts
  ti[,`:=`(script = gsub("(S(\\d|_|-|\\.)+)-.*", "\\1", script))]
  if (FLAGshortenIdentical) {
    ti[,`:=`(NIdentical = 1:.N,
             isMaxIdentical = 1:.N == .N), by = c("templateName", "templateVersion", "templateComment")]
    ti <- ti[NIdentical <= 3 | isMaxIdentical]
    ti[NIdentical > 3,`:=`(script = paste0("... ", script))]
  }
  ti <- ti[,list(script = paste0(script, collapse = ", ")), by = c("templateName", "templateVersion", "templateComment")]
    
  data.table::setcolorder(ti, c("templateName", "templateVersion", "script", "templateComment"))
  cfoutput_MdTable(ti, "templateName")
  invisible(ti)
}

#' Remove scripts AND their output folders
#'
#' @param filenames_vector 
#'
#' @return
#' @export
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#'
#' @examples
cf_script_remove <- function(filenames_vector) {
  for (from in filenames_vector){
    if (from != basename(from)) stop("Renaming output folder only works if getwd() = scriptdir")
    unlink(from)
    fromOut <- file.path("../04-Output", gsub(".R$", "", from))
    unlink(fromOut, recursive = TRUE)
  }
}


#' Title
#'
#' @return list of stale scripts and stale folders
#' @export
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#'
#' @examples
cf_script_identifyStale <- function() {
  from <- list.files(".", "\\.R")
  outFolders_Possible <- gsub(".R$", "", from)
  outFolders_Actual   <- list.dirs("../04-Output", full.names = FALSE, recursive = FALSE)
  
  cat("\n======= STALE SCRIPTS: setdiff(possible,actual): ",length(setdiff(outFolders_Possible,outFolders_Actual))," ========== \n")
  staleScripts <- setdiff(outFolders_Possible,outFolders_Actual)
  cat(staleScripts, sep = "\n")
  cat("\n======= STALE FOLDERS: setdiff(actual,possible): ",length(setdiff(outFolders_Actual,outFolders_Possible))," ========== \n")
  staleFolders <- setdiff(outFolders_Actual,outFolders_Possible)
  cat(staleFolders, sep = "\n")
  list(staleFolders = file.path("../04-Output", staleFolders), staleScripts = staleScripts)
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
      "  * Step back and think: How much time is the data really worth investing? Is it better to draw a line?",
      "* Coding",
      "  * Separate data programming and modeling", 
      "  * Collect all information for a problem in a list",
      "  * Functions should not be longer than 40-50 lines",
      "  * If you develop a modeling script along with data programming, that's fine, but the script should be split afterwards.",
      "  * Don't handle more than 3 data.tables at once. If more, go back and think, is it necessary?",
      "  * Use check()",
      "* Results",
      "  * Produce quality plots from the start and explain them, IQslides with plot and IQbulletlist might help.",
      "  * Results are single, specific statements. They are not endless pdf files full of plots.",
      "  * Produce communicatable results",
      "* Goal",
      "  * If you never did an analysis before, try mapping the necessary steps in advance",
      "  * Work out the fastest route to your goal and don't let yourself be distracted",
      "  * Think of agile development techniques - sprints and long-term goals.",
      "* Copy these tips into the script to not forget them :)",
      
      #  "* Principles of data programming",
      #  "  * Think before you do anything",
      #  "    * What is the dimensionality of the problem. Name all variables!",
      #  "    * For which tasks is long/wide data better",
      #  "    * How can two or more tables be merged",
      #  "  * First work through the column names",
      #  "    * Which ones to keep? Drop all the others! ",
      #  "    * The kept ones: Rename them to the most simple descriptive tag",
      #  "    * Which information is still missing? add it!",
      #  "  * Add a ROWID - column, but only if there is not already a uniquely identifying row (e.g. 'Protein ID')",
      #  "  * Malformed original values can also be replaced manually in the excel sheet (make a copy)",
      
      sep = "\n")
}

