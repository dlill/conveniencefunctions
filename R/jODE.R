#' Create an index-based version of an est_grid
#'
#' Used mainly internally in jODE_prepare_matrices
#'
#' @param est_grid,est_vec as in IQRtools, but est_vec should be ordered
#'
#' @return an est_grid with indices instead of names
#' @export
jODE_indify_est_grid <- function(est_grid, est_vec) {
  if (!identical(names(est_vec), sort(names(est_vec))) )
    stop("Names of est_vec must be ordered")
  positions <- setNames(1:length(est_vec), names(est_vec))
  ID <- est_grid[["ID"]]
  est_grid_pos <- as.data.frame(lapply(est_grid[!(names(est_grid) %in% c("ID"))], function(.x) positions[.x]))
  cbind(ID, est_grid_pos) # data.frame
} 


#' Turn fixed_grid and est_grid in numerical matrices
#'
#' @param est_grid,fixed_grid,fixed_grid as in IQRtools. "condition" will be removed
#' 
#' @return list of est_mat and fixed_mat
#' 
#' est_mat will be indified \link{jODE_indify_est_grid}
#'   
#' @export
jODE_prepare_matrices <- function(est_grid, fixed_grid, est_vec) {
  
  est_grid$condition <- fixed_grid$condition <- NULL
  
  est_grid <- jODE_indify_est_grid(est_grid, est_vec)
  est_mat <- as.matrix(est_grid)
  fixed_mat <- as.matrix(fixed_grid)
  list(est_mat = est_mat, fixed_mat = fixed_mat)
}

#' Generate Julia source code of prediction function
#'
#' @param est_mat,fixed_mat output of \link{jODE_prepare_matrices}
#' @param trafo,odes,obs,err symbolic definitions of functions. 
#' * Trafo is a general trafo valid for all conditions (as in P_indiv)
#' * odes need to be in eqnvec format
#'
#' @return character vector of the functions p, p2, f, f2, g, e, prd_condition (prd for a signle condition)
#' @export
jODE_funJ <- function(est_mat, fixed_mat, trafo, odes, obs, err) {
  
  nm_pars   <- colnames(est_mat)[-1]
  nm_fixed  <- colnames(fixed_mat)[-1]
  nm_u0     <- names(odes)
  nm_pinner <- setdiff(names(trafo), names(odes))
  
  jODE_indexInfo <- paste0(
    "# pars  ------------------------------------------", "\n", 
    "# ", paste0(paste0(1:length(nm_pars),   " = ", nm_pars), collapse = "; "), "\n",
    "# ", "\n",
    "# fixed   ------------------------------------------", "\n",
    "# ", paste0(paste0(1:length(nm_fixed),  " = ", nm_fixed), collapse = "; "), "\n",
    "# ", "\n",
    "# u0  ------------------------------------------", "\n",
    "# ", paste0(paste0(1:length(nm_u0),     " = ", nm_u0), collapse = "; "), "\n",
    "# ", "\n",
    "# pinner  ------------------------------------------", "\n",
    "# ", paste0(paste0(1:length(nm_pinner), " = ", nm_pinner), collapse = "; "), "\n"
  )
  
  # .. jODE_p ----
  # [] Add small documentation
  # * Might be slightly less efficient than the old version, or it is better, who knows
  jODE_p <- paste0(
    "function jODE_p(pars, fixed)", "\n",
    "    # Pars \n",
    "    ", paste0(nm_pars, collapse = ", ") , " = pars", "\n",
    "    # Fixed \n",
    "    ", paste0(nm_fixed, collapse = ", ") , " = fixed", "\n",
    " \n",
    "    # Trafo Equations \n",
    "    ", paste0(names(trafo), " = ", trafo, collapse = "\n    ")," \n",
    " \n",
    "    # u0 and pinner \n",
    "    u0 = [",     paste0(nm_u0, collapse = "; "), "]", "\n",
    "    pinner = [", paste0(nm_pinner, collapse = "; "), "]", "\n",
    " \n",
    "    return u0, pinner", "\n",
    "end # might be less efficient but clearer to read. Furthermore, I don't think this part will be the bottleneck", "\n"
  )
  
  # .. jODE_p2 ----
  # * Old version with indexing
  trafo_final <- replaceSymbols(c(nm_fixed, nm_pars), paste0("pouter[",1:(length(nm_fixed)+length(nm_pars)),",:]"), trafo)
  trafo_final <- str_replace_all(trafo_final, c( # now it gets ugly: need to transform operators to vectorized form since ForwardDiff.jl needs it
    "\\bexp\\b" = "exp.",
    "\\blog\\b" = "log.",
    "\\b\\+\\b" = ".+",
    "\\b-\\b" = ".-",
    "\\b\\*\\b" = ".*",
    "\\b\\^\\b" = ".^"
  ))
  trafo_final <- setNames(trafo_final, paste0("pinner[",1:length(trafo_final),",:]"))
  u0_ind <- match(names(odes), names(trafo))
  pinner_ind <- setdiff(1:length(trafo), u0_ind)
  jODE_p2 <- paste0(
    "function jODE_p2(pars, fixed)", "\n",
    "    pouter = [pars...; fixed...]", "\n",
    "    myorder = ", "[",paste0(order(c(nm_pars, nm_fixed)), collapse = "; "),"]", "\n",
    "    pouter = [pouter[i] for i in myorder]", "\n",
    "    pinner = convert.(eltype(pars),zeros(", length(trafo_final),"))", "\n",
    "    ", paste0(names(trafo_final), " = ", trafo_final, collapse = "\n    ")," \n",
    "    u0_ind = [", paste0(u0_ind, collapse = "; "), "]", "\n",
    "    pinner_ind = setdiff([1:length(pinner)...],u0_ind)", "\n",
    "    return pinner[u0_ind], pinner[pinner_ind]", "\n",
    "end", "\n"
  )
  
  
  # .. jODE_f ----
  jODE_f <- paste0(
    "jODE_f = @ode_def begin", "\n",
    "    ", paste0("d", names(odes), " = ", odes, collapse = "\n    ")," \n",
    "end ", paste0(setdiff(names(trafo), names(odes)), collapse = " "), "\n",
    ""
  )
  # .. jODE_f2 ----
  jODE_f2 <- paste0(
    "function jODE_f2(du, u, p,t)", "\n",
    "    # Get u0 and p \n",
    "    ", paste0(names(odes), collapse = ", "), " = u"," \n",
    "    ", paste0(setdiff(names(trafo), names(odes)), collapse = ", "), " = p"," \n",
    "    # ODEs \n",
    "    ", paste0("du[", 1:length(odes), "]", " = ", odes, collapse = "\n    "), " \n",
    "    # Output to look at solution \n",
    "    du", "\n",
    "end ", "\n",
    ""
  )
  
  # .. jODE_g ----
  jODE_g <- paste0(
    "function jODE_g(sol, pinner)", "\n",
    "    # Get solutions of odes", "\n",
    "    ", paste0(names(odes), " = sol[", 1:length(odes), ",:]", collapse = "\n    ")," \n",
    "    # Get Parameters", "\n",
    "    ", paste0(setdiff(names(trafo), names(odes)), collapse = ","), " = pinner ", " \n",
    "    # Pre-assign output-vectors", "\n",
    "    ", paste0(names(obs), " = convert.(eltype(pinner),zeros(length(sol.t)))", collapse = "\n    ")," \n",
    "    # Evaluate observations", "\n",
    "    ", paste0("@. ", names(obs), " = ", obs, collapse = "\n    ")," \n",
    "    # Collect observations", "\n",
    "    return [", paste0(names(obs), collapse = " "), "]", " \n",
    "end", "\n"
  )
  
  # .. jODE_e ----
  jODE_e <- paste0(
    "function jODE_e(obs, pinner)", "\n",
    "    # Get observations", "\n",
    "    ", paste0(names(obs), " = obs[:,", 1:length(obs), "]", collapse = "\n    ")," \n",
    "    # Get Parameters", "\n",
    "    ", paste0(setdiff(names(trafo), names(odes)), collapse = ","), " = pinner ", " \n",
    "    # Evaluate error models", "\n",
    "    ", paste0("@. ", names(err), " = ", err, collapse = "\n    ")," \n",
    "    # Collect errors", "\n",
    "    return [", paste0(names(err), collapse = " "), "]", " \n",
    "end", "\n"
  )
  
  
  # .. jODE_prd_condition ----
  jODE_prd_condition <- paste0('function jODE_prd_condition(pars, fixed, ID, datatimes, 
    jODE_p, jODE_f, jODE_g, jODE_e)
    # Prediction function for single condition
    
    # Parameters
    u0, pinner = jODE_p(pars, fixed)
    # ODE
    # prob = ODEProblem(jODE_f, u0, (min(0, datatimes...), max(datatimes...)), pinner) # [] decide on version. this one allows negative starting times
    prob = ODEProblem(jODE_f, u0, max(datatimes...), pinner)
    # [] Play around with different solvers?
    sol  = solve(prob, saveat = datatimes, reltol = 1e-12, abstol = 1e-12)
    # Observation
    obs = jODE_g(sol, pinner)
    # Error model
    err = jODE_e(obs, pinner)
    
    # DataFrame for output 
    df_names = [', paste0('"', names(obs), '"', collapse = "; ") ,'; "time"; "ID"]
    df = DataFrame([obs datatimes ones(length(datatimes))*ID])
    names!(df, Symbol.(df_names))
    df = stack(df, 1:',length(obs),', variable_name = :name)
      
    df_err_names = [', paste0('"', names(err), '"', collapse = "; ") ,'; "time"; "ID"]
    df_err = DataFrame([err datatimes ones(length(datatimes))*ID])
    names!(df_err, Symbol.(df_err_names))
    df_err = stack(df_err, 1:',length(err),', value_name = :sigma, variable_name = :name)
    
    df_out = join(df, df_err, on = [:name,:time,:ID], kind = :outer)
    df_out.name = String.(df_out.name)
    df_out
end # Prediction function for single ID
')
  
  # .. return ----  
  c(jODE_indexInfo = jODE_indexInfo,
    jODE_p = jODE_p, 
    jODE_p2 = jODE_p2, 
    jODE_f = jODE_f, 
    jODE_f2 = jODE_f2, 
    jODE_g = jODE_g, 
    jODE_e = jODE_e, 
    jODE_prd_condition = jODE_prd_condition)
}

#' Conveniencefunction to output source file without having to know the data yet
#'
#' @param est_grid,fixed_grid,fixed_grid as in IQRtools
#' @param trafo,odes,obs,err symbolic functions as in dMod
#' @param jODE_sourcefile path where julia file shall be written
#'
#' @return called for side effect. source file path is returned invisibles
#' @export
jODE_write_sourcefile <- function(est_grid, fixed_grid, est_vec, trafo, odes, obs, err, 
                                  jODE_sourcefile = tempfile("jODE_source_file", 
                                                             fileext = ".jl")) {
  
  grid_matrices <- jODE_prepare_matrices(est_grid, fixed_grid, est_vec)
  est_mat   <- grid_matrices[[1]]
  fixed_mat <- grid_matrices[[2]]
  
  julia_functions <- jODE_funJ(
    est_mat = est_mat, 
    fixed_mat = fixed_mat, 
    trafo = trafo, 
    odes = odes, 
    obs = obs, 
    err = err)
  
  try(unlink(jODE_sourcefile), silent = TRUE)
  writeLines(julia_functions, jODE_sourcefile, sep = "\n\n\n\n")
  
  invisible(jODE_sourcefile)
}


#' Output a little test script to play around with current objects
#'
#' @param est_mat,fixed_mat output of \code{\link{jODE_prepare_matrices}}
#' @param trafo,odes,obs,err symbolic definitions
#' @param est_vec sorted est_vec
#'
#' @return string which you can use to write a file
#' @export <- <- 
jODE_writeTestScript <- function(est_mat, fixed_mat, trafo, odes, obs, err, est_vec) {
  nm_pars   <- colnames(est_mat)[-1]
  nm_fixed  <- colnames(fixed_mat)[-1]
  nm_u0     <- names(odes)
  nm_pinner <- setdiff(names(trafo), names(odes))
  
  jODE_indexInfo <- paste0(
    "# pars  ------------------------------------------", "\n",
    "# ", paste0(paste0(1:length(nm_pars),   " = ", nm_pars), collapse = "; "), "\n",
    "# ", "\n",
    "# fixed   ------------------------------------------", "\n",
    "# ", paste0(paste0(1:length(nm_fixed),  " = ", nm_fixed), collapse = "; "), "\n",
    "# ", "\n",
    "# u0  ------------------------------------------", "\n",
    "# ", paste0(paste0(1:length(nm_u0),     " = ", nm_u0), collapse = "; "), "\n",
    "# ", "\n",
    "# pinner  ------------------------------------------", "\n",
    "# ", paste0(paste0(1:length(nm_pinner), " = ", nm_pinner), collapse = "; "), "\n"
  )
  jODE_libraries <- paste0(c(
    "# -------------------------------------------------------------------------#",
    "# Libraries  ----",
    "# -------------------------------------------------------------------------#",
    "using JLD2",
    "using CSV",
    "using ParameterizedFunctions",
    "using DifferentialEquations",
    "using DataFrames",
    "using DataFramesMeta",
    "using StatsFuns",
    "using ForwardDiff",
    "using DiffResults"), collapse = "\n")
  
  jODE_fixed_functions <-
    paste0(c(
      # 'cd("/home/daniel/Promotion/Promotion/Projects/LiSyM/TGFb/Work/02-Scripts")',
      paste0('include("',system.file('jODE/jODE_v2_002_fixed_functions.jl', package = "conveniencefunctions"), '")'),
      'include("S102-Julia01_functions.jl")',
      'data = CSV.read("../01-Data/001-a-data_full.csv")',
      'data = jODE_sanitizeData(data)'
    ), collapse = "\n")
  
  jODE_fixed_mat <- fixed_mat %>% apply(1, paste0, collapse = " ") %>% paste0(collapse = "; \n") %>% paste0("fixed_mat = [", . , "]")
  jODE_est_mat <- est_mat %>% apply(1, paste0, collapse = " ") %>% paste0(collapse = "; \n") %>% paste0("est_mat = [", . , "]")
  jODE_est_vec <- est_vec %>% paste0(collapse = " ") %>% paste0("est_vec = [", . , "]")
  
  jODE_conditions <- est_mat[,1] %>%  paste0(collapse = " ") %>% paste0("IDs = [", . , "]")
  jODE_condition <- "ID = IDs[1]"
  
  
  jODE_testcode <-
    paste0(c(
      "# pars, fixed = jODE_make_pars(est_vec, est_mat, fixed_mat, ID)",
      "# datatimes = unique(data.time)",
      "# prob = ODEProblem(jODE_f2, u0, max(datatimes...), pinner)",
      "# sol  = solve(prob, BS3(), saveat = datatimes)",
      "# jODE_prd_condition(pars, fixed, ID, datatimes, jODE_p, jODE_f, jODE_g, jODE_e)",
      "",
      "obj = jODE_normL2(data, jODE_prd_condition, est_mat,",
      "                    fixed_mat, jODE_make_pars, jODE_p, jODE_f, ",
      "                    jODE_g, jODE_e",
      ")",
      "@time obj(est_vec, deriv = false, verbose = true)"
    ), collapse = "\n")
  
  
  c(jODE_indexInfo, jODE_libraries, jODE_fixed_functions, jODE_fixed_mat, jODE_est_mat, jODE_est_vec, jODE_conditions, jODE_condition, jODE_testcode) %>%
    paste0(collapse = "\n\n")
  
}




#' normL2 for julia interface
#'
#' @param est_grid,fixed_grid,est_vec as in IQRtools
#' @param trafo,odes,obs,err symbolic definitions of functions as in dMod
#' @param jODE_sourcefile the intermediate file in which the julia-code is written
#' @param data filepath or data.frame
#' @param sigma character to paste into julia command. must be one of ":sigma_1" (from data or errormodel) or ":sigma" (from errormodel or data) [] Check which is which
#'
#' @return objective function
#' @export
jODE_normL2 <- function(est_grid, fixed_grid, est_vec, trafo, odes, obs, err, 
                        jODE_sourcefile = tempfile("jODE_source_file", fileext = ".jl"), 
                        data, sigma = ":sigma_1") {
  
  # Load functions in Julia
  jODE_write_sourcefile(est_grid   = est_grid, 
                        fixed_grid = fixed_grid, 
                        est_vec    = est_vec, 
                        trafo      = trafo, 
                        odes       = odes, 
                        obs        = obs, 
                        err        = err, 
                        jODE_sourcefile = jODE_sourcefile)
  julia$source(jODE_sourcefile)
  
  # Assign parameter vs condition grids to Julia
  grid_matrices <- jODE_prepare_matrices(est_grid, fixed_grid, est_vec)
  est_mat       <- grid_matrices[[1]]
  fixed_mat     <- grid_matrices[[2]]
  julia$assign("est_mat",   est_mat)
  julia$assign("fixed_mat", fixed_mat)
  
  # Assign data in Julia
  if (is.character(data)){
    julia$command(paste0('data = CSV.read("', data, '", typemap = Dict(Int64 => Float64))'))
  } else {
    julia$assign("data", data)
  }
  
  # dirty bugfix
  # [] why is this "dirty", can I just omit it?
  julia$assign("est_vec", est_vec)
  
  julia$command(paste0("obj = jODE_normL2(data, jODE_prd_condition, est_mat,
    fixed_mat, jODE_make_pars, jODE_p, jODE_f, jODE_g, jODE_e, ",sigma,")"))
  
  # @param pars
  # @FLAGnearPD
  # [] add params fixed and conditions
  myobjfun <- function(pars, FLAGnearPD = FALSE) {
    julia$assign("pars", pars)
    myobjlist <- julia$eval("obj_result = obj(pars);")
    myobjlist <- do.call(objlist, myobjlist)
    if (FLAGnearPD)
      myobjlist$hessian <- as.matrix(Matrix::nearPD(myobjlist$hessian)$mat)
    names(myobjlist$gradient) <- names(pars)
    dimnames(myobjlist$hessian) <- lapply(1:2, function(x) names(pars))
    myobjlist
  }
  
  return(myobjfun)
}


#' Setup Julia for the session and load some jODE functions which don't change
#'
#' @param juliaPath path to julia installation
#'
#' @export
jODE_setup <- function(juliaPath = "/home/daniel/Julia/julia-1.1.1/bin") {
  if (!"JuliaCall" %in% loadedNamespaces()){
    require("JuliaCall")
    message("Julia was loaded\n")
    }
  julia <- julia_setup(juliaPath)
  
  julia$source("inst/jODE/jODE_v2_001_libraries.jl")
  julia$source("inst/jODE/jODE_v2_002_fixed_functions.jl")
}
