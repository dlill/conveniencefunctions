
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
dMod_files <- function(path, identifier = "") {
  list(
    mstrust   = file.path(path, "Results", "mstrust", paste0("mstrustList-",identifier,".rds")),
    profile   = file.path(path, "Results", "profile", paste0("profiles-",identifier,".rds")),
    L1        = file.path(path, "Results", "L1", paste0("L1-",identifier,".rds")),
    petabdMod = file.path(path, paste0("pd",".rds"))
  )
}



#' Consistently save profiles
#'
#' @param profiles 
#' @param path 
#'
#' @return
#' @export
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#'
#' @examples
dMod_saveProfiles <- function(profs, path) {
  profsplit <- split(profs, profs$whichPar)
  lapply(profsplit, function(prof) {
    whichNm <- as.character(unique(prof$whichPar))
    filename <- dMod_files(path, whichNm)$profile
    dir.create(dirname(filename), FALSE, TRUE)
    saveRDS(prof, filename)
    prof
  })
  invisible()
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


#' Title
#'
#' @param fit 
#' @param path 
#' @param identifier 
#' @param FLAGoverwrite 
#'
#' @return
#' @export
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#'
#' @examples
dMod_saveMstrust <- function(fit, path, identifier = "1", FLAGoverwrite = FALSE) {
  if (!is.parframe(fit)) fit <- cf_as.parframe(fit)
  
  filename <- dMod_files(path, identifier)$mstrust
  if (!FLAGoverwrite && file.exists(filename)) 
    stop("FLAGoverwrite is FALSE and file.exists: ", filename)
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
  saveRDS(fit, filename)
}


#' Title
#'
#' @param path 
#'
#' @return
#' @export
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#'
#' @examples
dMod_readMstrust <- function(path, identifier = NULL) {
  filename <- dMod_files(path, identifier)$mstrust
  fits <- readRDS(filename)
  fits
}


#' Title
#'
#' @param fit 
#' @param path 
#' @param identifier 
#' @param FLAGoverwrite 
#'
#' @return
#' @export
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#'
#' @examples
dMod_saveL1 <- function(L1, path, identifier = "1", FLAGoverwrite = FALSE) {
  filename <- dMod_files(path, identifier)$L1
  if (!FLAGoverwrite && file.exists(filename)) 
    stop("FLAGoverwrite is FALSE and file.exists: ", filename)
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
  saveRDS(L1, filename)
}


#' Title
#'
#' @param path 
#'
#' @return
#' @export
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#'
#' @examples
dMod_readL1 <- function(path) {
  filename <- dMod_files(path)$L1
  fits <- list.files(dirname(filename), "rds$", full.names = TRUE)
  fits <- lapply(fits, readRDS)
  fits <- do.call(rbind, fits)
  fits
}




# -------------------------------------------------------------------------#
# Profiles ----
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
                       method = c("optimize"),
                       stepControl = list(limit = 40, min = log10(1.005), stepsize = log10(1.005)), 
                       algoControl = NULL,
                       optControl  = NULL,
                       verbose = FALSE,
                       cores = 1,
                       path = tempdir(),
                       FLAGoverwrite = FALSE,
                       cautiousMode = TRUE,
                       ...) {
  
  # Ensure whichPar is character
  if (is.numeric(whichPar)) whichPar <- names(pars)[whichPar %in% names(pars)]
  
  if (!FLAGoverwrite) {
    files <- vapply(whichPar, function(id) dMod_files(path, id)$profile, "path/to/file")
    parExists <- whichPar[file.exists(files)]
    if (length(parExists)) cat("The following profiles exist and are not calculated: \n", 
                               capture.output(dput(parExists)))
    whichPar <- setdiff(whichPar, parExists)
  }
  whichNm <- whichPar[[1]]
  
  ncores <- cores
  # parallel::mclapply(X = whichPar, mc.cores = ncores, FUN = function(whichNm) {
  parallel::mclapply(X = whichPar, mc.cores = ncores, FUN = function(whichNm) {
    
    filename <- dMod_files(path, whichNm)$profile
    dir.create(dirname(filename), FALSE, TRUE)
    
    prof <- try(dMod::profile(obj, pars, whichPar = whichNm, alpha, 
                              limits,method,stepControl,
                              algoControl,optControl,verbose,
                              cores = 1, cautiousMode = cautiousMode, ...))
    
    saveRDS(prof, filename)
    prof
  })
  
}



# -------------------------------------------------------------------------#
# runbg ----
# -------------------------------------------------------------------------#
#' Title
#'
#' @param FLAGjobPurged 
#' @param FLAGjobDone 
#' @param FLAGjobRecover 
#'
#' @return
#' @export
#'
#' @examples
printJobInfo <- function(FLAGjobPurged, FLAGjobDone, FLAGjobRecover) {
  if (FLAGjobPurged) {
    msg <- "Job is done and purged"
  } else if (FLAGjobDone) {
    msg <- "Job is done but not yet purged"
  } else if (FLAGjobRecover) {
    msg <- "Job is probably active"
  } else msg <- "Job will be created"
  cat("===============================",msg,
      paste0("purged :", as.character(FLAGjobPurged)),
      paste0("done   :", as.character(FLAGjobDone)),
      paste0("recover:", as.character(FLAGjobRecover)),
      "===========================\n", sep = "\n")
}
