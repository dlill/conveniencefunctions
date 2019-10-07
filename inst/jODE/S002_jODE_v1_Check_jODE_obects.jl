#-------------------------------------------------------#
# 1 Set working directory ----
#-------------------------------------------------------#
cd("/home/daniel/Promotion/Promotion/Projects/jODE/Work/02-Scripts/Julia")

#-------------------------------------------------------#
# 2 Libraries ----
#-------------------------------------------------------#

#-------------------------------------------------------#
# 3 dMod-objects to test in this script ----
#-------------------------------------------------------#
include("Resources/jODE_v1_001_libraries.jl");
include("Resources/jODE_v1_002_fixed_functions.jl");
include("Resources/jODE_v1_003_dynamic_functions.jl");
data = CSV.read("../../01-Data/Model1/M1_data_2_subj_no_bloq/M1_data_2_subj_no_bloq_data.csv", typemap = Dict(Int64 => Float64))
wup = load("../../01-Data/Model1/M1_data_2_subj_no_bloq/M1_data_2_subj_no_bloq.jld")
fixed_mat, est_mat, est_vec, condition, conditions = values(wup)

pars, fixed = jODE_v1_make_pars(est_vec, est_mat, fixed_mat, condition)
datatimes = unique(data.time)

prob = ODEProblem(jODE_v1_f2, u0, max(datatimes...), pinner)

sol  = solve(prob, BS3(), saveat = datatimes)
sol  = solve(prob, DP5(), saveat = datatimes)
sol  = solve(prob, Tsit5(), saveat = datatimes)
sol  = solve(prob, Rosenbrock23(), saveat = datatimes) # Wfact
sol  = solve(prob, VCABM(), saveat = datatimes)
sol  = solve(prob, QNDF(), saveat = datatimes)
sol  = solve(prob, Rodas4(), saveat = datatimes)
sol  = solve(prob, Rosenbrock32(), saveat = datatimes)
      


jODE_v1_prd_condition(pars, fixed, condition, datatimes, jODE_v1_p, jODE_v1_f, jODE_v1_g, jODE_v1_e)

obj = jODE_v1_normL2(data, jODE_v1_prd_condition, est_mat,
fixed_mat, jODE_v1_make_pars, jODE_v1_p, jODE_v1_f, 
jODE_v1_g, jODE_v1_e, :sigma_1
)

obj(est_vec)
