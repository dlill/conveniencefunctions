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
include("Resources/jODE_v2_001_libraries.jl");
include("Resources/jODE_v2_002_fixed_functions.jl");
include("Resources/jODE_v2_003_dynamic_functions.jl");
# 2 subjects
data = CSV.read("../../01-Data/Model1/M1_data_2_subj_no_bloq/M1_data_2_subj_no_bloq_data.csv", typemap = Dict(Int64 => Float64))
wup = load("../../01-Data/Model1/M1_data_2_subj_no_bloq/M1_data_2_subj_no_bloq.jld")


#=
# # 10 subjects
# data = CSV.read("../../01-Data/Model1/M1_data_10_subj_/M1_data_10_subj__data.csv", typemap = Dict(Int64 => Float64))
# wup =      load("../../01-Data/Model1/M1_data_10_subj_/M1_data_10_subj_.jld")
=#

#=
# 50 subjects
data =  CSV.read("../../01-Data/Model1/M1_data_50_subj_/data.csv", typemap = Dict(Int64 => Float64))
wup =       load("../../01-Data/Model1/M1_data_50_subj_/M1_data_50_subj_.jld")
=#

fixed_mat, est_mat, est_vec, condition, conditions = values(wup)

pars, fixed = jODE_v2_make_pars(est_vec, est_mat, fixed_mat, condition)
datatimes = unique(data.time)
jODE_v2_prd_condition(pars, fixed, condition, datatimes, jODE_v2_p, jODE_v2_f, jODE_v2_g, jODE_v2_e)

include("Resources/jODE_v2_002_fixed_functions.jl");
obj = jODE_v2_normL2(data, jODE_v2_prd_condition, est_mat, fixed_mat, jODE_v2_make_pars, jODE_v2_p, jODE_v2_f,  jODE_v2_g, jODE_v2_e, :sigma_1)
obj(est_vec, verbose = true)



breakpoint(jODE_v2_g)
breakpoint(jODE_v2_prd_condition)
@enter obj(est_vec, verbose = true)
obj(est_vec, verbose = true)
obj(est_vec, verbose = true, deriv = false)


obj2 = jODE_v2_normL2(data, jODE_v2_prd_condition, est_mat,
fixed_mat, jODE_v2_make_pars, jODE_v2_p, jODE_v2_f2, 
jODE_v2_g, jODE_v2_e, :sigma)

obj2(est_vec)




jODE_v2_LL_condition(pars)










#=
# Try using Zygote.jl

using Zygote
"""
Convenience function to evaluate function gradient and hessian (fgh) of a scalar function with zygote 
"""
function fgh_zygote(f, x::AbstractArray)
    val, grd, hes = 0., zeros(length(x)), zeros(length(x),length(x))
    val = f(x)
    grd, hes = Zygote.forward_jacobian(x -> Zygote.gradient(f, x)[1], x)
    (val, grd, hes)
end


""" 
sigma: symbol, choose between :sigma (take sigma from data) and :sigma_1 (take sigma from error model)
normL2 with zygote
"""
function jODE_v2_normL2_zygote(data, jODE_v2_prd_condition, est_mat,
    fixed_mat, jODE_v2_make_pars, jODE_v2_p, jODE_v2_f, jODE_v2_g, jODE_v2_e,
    sigma
    )
    
    datatimes = convert.(Float64, unique(data.time))
    conditions = unique(data.condition)
    pars, fixed = jODE_v2_make_pars(est_vec, est_mat, fixed_mat, 1)
    
    cn = ones(1)*conditions[1]
    
    function jODE_v2_LL_condition(pars)
        mycn = cn[1]
        dummy, fixed = jODE_v2_make_pars(est_vec, est_mat, fixed_mat, mycn)
        pred = jODE_v2_prd_condition(pars, fixed, mycn, datatimes, jODE_v2_p, jODE_v2_f, jODE_v2_g, jODE_v2_e) 
        # prepare calculations
        df_both = join(data,pred, on = [:name, :time, :condition], kind = :inner, makeunique = true)
        df_both = @transform(df_both, is_bloq = :value .< :lloq, objval = zeros(length(:lloq)))
        df_both = sort(df_both, cols = :is_bloq)
        
        # split into aloq and bloq
        if any(df_both.is_bloq)
            df_aloq, df_bloq = groupby(df_both, :is_bloq)
        else 
            df_aloq = df_both
        end
        
        # calculate LL
        # [] Include option to take sigma from data
        # [] Include opt.M4:
        df_aloq.objval = @. log(2π) + 2*log(df_aloq[sigma]) + ((df_aloq[:value_1]-df_aloq[:value])/df_aloq[sigma])^2
        if @isdefined df_bloq
            df_bloq.objval = @. -2*StatsFuns.normlogcdf(df_bloq[:value_1], sigma, df_bloq[:lloq])
        end
        
        # Objective value
        # summing df_both works because df_aloq and df_bloq are not deep copies
        sum([df_both.objval...])
        
    end
    
    # Calculate with derivs
    function jODE_v2_LL_deriv(est_vec)
        
        val, grd, hes = 0., zeros(length(est_vec)), zeros(length(est_vec),length(est_vec))
        pars, dummy2 = jODE_v2_make_pars(est_vec, est_mat, fixed_mat, cn)
        val_interm, grd_interm, hes_interm = [0.], zeros(length(est_vec)), zeros(length(est_vec),length(est_vec))
        for condition in conditions 
            cn[1] = condition
            pars, dummy2 = jODE_v2_make_pars(est_vec, est_mat, fixed_mat, condition)
            val_interm[:], grd_interm[:], hes_interm[:] = fgh_zygote(jODE_v2_LL_condition, pars) 
            val += val_interm[1]
            indices = est_mat[convert(Int64,condition), 2:end]
            grd[indices] += [grd_interm...;]
            hes[indices, indices] += hes_interm
        end

        return (val, grd, hes)
    end

end

obj3 = jODE_v2_normL2_zygote(data, jODE_v2_prd_condition, est_mat,
fixed_mat, jODE_v2_make_pars, jODE_v2_p, jODE_v2_f, 
jODE_v2_g, jODE_v2_e, :sigma)

obj3(est_vec)

=#

