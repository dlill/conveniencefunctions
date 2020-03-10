#-------------------------------------------------------#
# 1 Set working directory ----
#-------------------------------------------------------#
cd("/home/daniel/Promotion/Promotion/Projects/jODE/Work/02-Scripts/Julia")

#-------------------------------------------------------#
# 2 load Objects ----
#-------------------------------------------------------#
include("Resources/jODE_v2_001_libraries.jl");
include("Resources/jODE_v2_002_fixed_functions.jl");
include("Resources/jODE_v2_003_dynamic_functions.jl");
data = CSV.read("../../01-Data/Model1/M1_data_2_subj_no_bloq/M1_data_2_subj_no_bloq_data.csv", typemap = Dict(Int64 => Float64))
wup = load("../../01-Data/Model1/M1_data_2_subj_no_bloq/M1_data_2_subj_no_bloq.jld")
fixed_mat, est_mat, est_vec, condition, conditions = values(wup)


#-------------------------------------------------------#
# 3 Simulate once ----
#-------------------------------------------------------#
pars, fixed = jODE_v2_make_pars(est_vec, est_mat, fixed_mat, condition)
datatimes = unique(data.time)
jODE_v2_prd_condition(pars, fixed, condition, datatimes, jODE_v2_p, jODE_v2_f, jODE_v2_g, jODE_v2_e)


fixed[2] = 0 # Ad



function condition1(u,t,integrator)
    t
end
function affect1!(integrator)
    integrator.p[11] = 1e6
    set_proposed_dt!(integrator,1e-10)
end
cb1 = ContinuousCallback(condition1,affect1!)

function condition2(u,t,integrator)
    t - 1e-4
end
function affect2!(integrator)
    integrator.p[11] = integrator.p[11]*0
    set_proposed_dt!(integrator,1e-10)
end
cb2 = ContinuousCallback(condition2,affect2!)

cb = CallbackSet(cb1, cb2)


u0, p = jODE_v2_p(pars, fixed)

p[11] = 0
p[4] = 1e-6
prob = ODEProblem(jODE_v2_f, u0, (-1e-6,max(datatimes...)), p)
sol  = solve(prob 
    # , alg = BS3()
    , callback = cb
    # , tstops = [0.;0.0001]
    # , tstops = [0.;0.0000999991;0.0001;0.00010000001]
    , tstops = [0.;0.0000999991;0.0001]
    )
p[11]
plot(sol)





#-------------------------------------------------------#
# 4 simpler system ----
#-------------------------------------------------------#


function f(du, u, p, t)
    du[1] = p[1]
end

function condition1(u,t,integrator)
    t - 1e-5
end
function affect1!(integrator)
    integrator.p[1] = 2
end
cb1 = ContinuousCallback(condition1,
    affect1!, 
    rootfind = true,
    abstol = 1e-10,
    reltol = 1e-10)

u0  = [0.]
p   = [1.]
prob = ODEProblem(f,u0, (0., 10.), p)
sol  = solve(prob, callback = cb1, tstops = [0;1e-5;1e-4;1e-3])
plot(sol)
p