
# -------------------------------------------------------------------------#
# Files ----
# -------------------------------------------------------------------------#
#' Consistent dMod filenames
#'
#' @param path 
#' @param identifier 
#'
#' @return
#' @export
#'
#' @examples
cf_dModFiles <- function(path, identifier = "") {
  list(
    mstrust = file.path(path, "Results", "mstrust", paste0("mstrustList-",identifier,".rds")),
    profile    = file.path(path, "Results", "profile", paste0("profiles-",identifier,".rds")),
    petabdMod    = file.path(path, paste0("pd-",identifier,".rds"))
  )
}


#' Title
#'
#' @param path 
#'
#' @return Profiles
#' @export
#'
#' @examples
dMod_readProfiles <- function(path = .outputFolder) {
  profPath <- file.path(path, "Results", "profile")
  files <- list.files(profPath, full.names =  TRUE)
  profs <- lapply(files, readRDS)
  
  hasError <- vapply(profs, function(x) inherits(x, "tre-error"), FALSE)
  if (any(hasError)) 
    cat("The following profiles have errors: \n",
        "* ", paste0(basename(files[hasError]), collapse = "\n* "))
  
  profs <- profs[!hasError]
  profs <- do.call(rbind, profs)
  profs
}



# -------------------------------------------------------------------------#
#  ----
# -------------------------------------------------------------------------#

#' Better wrapper around dMod::profile
#' 
#' Improvements
#' * Automatically save profiles with consistent filenames
#' * Run each profile in a try
#' 
#' inheritParams dMod::profile
#' 
#' @param outputFolder 
#'
#' @return list of profiles
#' @export
cf_profile <- function(obj, pars, whichPar, alpha = 0.05, 
                       limits = c(lower = -Inf, upper = Inf), 
                       method = c("integrate", "optimize"),
                       stepControl = NULL, 
                       algoControl = NULL,
                       optControl  = NULL,
                       verbose = FALSE,
                       cores = 1,
                       path = tempdir(),
                       FLAGoverwrite = FALSE,
                       ...) {
  
  # Ensure whichPar is character
  if (is.numeric(whichPar)) whichPar <- names(pars)[whichPar %in% names(pars)]
  
  if (!FLAGoverwrite) {
    files <- vapply(whichPar, function(id) cf_dModFiles(path, id)$profile, "path/to/file")
    parExists <- whichPar[file.exists(files)]
    if (length(parExists)) cat("The following profiles exist and are not calculated: \n", 
                               capture.output(dput(parExists)))
    whichPar <- setdiff(whichPar, parExists)
  }
  whichNm <- whichPar[[1]]
  
  ncores <- 1
  # parallel::mclapply(X = whichPar, mc.cores = ncores, FUN = function(whichNm) {
  parallel::mclapply(X = whichPar, mc.cores = ncores, FUN = function(whichNm) {
    
    filename <- cf_dModFiles(path, whichNm)$profile
    dir.create(basename(filename), FALSE, TRUE)
    
    prof <- try(dMod::profile(obj, pars, whichPar = whichNm, alpha, 
                              limits,method,stepControl,
                              algoControl,optControl,verbose,
                              cores = 1, ...))
    
    saveRDS(prof, filename)
    prof
  })
  
}

