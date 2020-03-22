# result = DiffResults.JacobianResult(est_vec)
# # result = DiffResults.DiffResult(est_vec)
# # result = DiffResults.GradientResult(est_vec)
# # result = DiffResults.HessianResult(est_vec)
# 
# result = DiffResults.value!(result, wupwup, est_vec)
# result = DiffResults.hessian!(result, wupwup, est_vec)
# # result = DiffResults.derivative!(result, wupwup, est_vec)
# # result = DiffResults.gradient!(result, wupwup, est_vec)
# # result = DiffResults.jacobian!(result, wupwup, est_vec)
# 
# DiffResults.value(result)
# DiffResults.jacobian(result)
# # DiffResults.derivative(result)
# # DiffResults.gradient(result)
# # DiffResults.hessian(result)






function wupwup(est_vec)
pars, fixed = jODE_make_pars(est_vec, est_mat, fixed_mat, condition)
u0, pinner = jODE_p(pars, fixed)

# [] Would be nice to get it working with ParameterizedFunctions
#jODE_f.f(convert.(eltype(pars),zeros(length(u0))), u0, pinner, 0.1)
#jODE_f2(convert.(eltype(pars),zeros(length(u0))), u0, pinner, 0.1)

# ODE
prob = ODEProblem(jODE_f2, u0, max(datatimes...), pinner)
sol  = solve(prob, saveat = datatimes)


# Observation
obs = jODE_g(sol, pinner)

# Error model
err = jODE_e(obs, pinner)

# DataFrame for output 
# [] This is the "R"-way, but is it also the Julian way?
df_names = [["OUTPUT$i" for i in 1:size(obs)[2]]...; "time"; "condition"]
df = DataFrame([obs datatimes ones(length(datatimes))*condition])
names!(df, Symbol.(df_names))
df = stack(df, 1:2, variable_name = :name)

df_err = DataFrame([err datatimes ones(length(datatimes))*condition])
names!(df_err, Symbol.(df_names))
df_err = stack(df_err, 1:2, value_name = :sigma, variable_name = :name)

df_out = join(df, df_err, on = [:name,:time,:condition])
df_out.name = String.(df_out.name)

return df_out.value, df_out.sigma, df_out[:, [:name, :time, :condition]]
end

wupwup(est_vec)
ForwardDiff.jacobian(wupwup, est_vec)





