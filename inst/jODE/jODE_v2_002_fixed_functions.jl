"""
Function to extract pars,fixed from est_vec, est_mat and fixed_mat for a secific ID

Use with output from R function jODE_01_prepare_mats

...
# Arguments
- `est_vec`: vector containing all outer parameters to be estimated
- `est_mat`: lookup table for pars to be estimated for specific ID. 
- `fixed_mat`: table containing fixed pars
- `ID`: ID identifier
...
"""
function jODE_make_pars(est_vec, est_mat, fixed_mat, ID)
    pars  = est_vec[est_mat[    est_mat[:,1] .== ID, 2:end]]
    fixed =      fixed_mat[fixed_mat[:,1] .== ID, 2:end]
    return(pars, fixed)
end # Make pars function: Create pars and fixed from pars, est_mat, fixed_mat, ID


"""
prediction function for all IDs

inheritParams jODE_normL2
"""
function jODE_prd_indiv(est_vec, IDs, datatimes, 
    # Prediction function for multiple IDs
    est_mat, fixed_mat, jODE_make_pars, jODE_p, jODE_f, jODE_g, jODE_e)
    function prd__(cond__) 
        pars, fixed = jODE_make_pars(est_vec, est_mat, fixed_mat, cond__)
        pred = jODE_prd_condition(pars, fixed, cond__, datatimes, jODE_p, jODE_f, jODE_g, jODE_e) 
    end
    out = [prd__(cond__) for cond__ in IDs]
    [out...;]
end # Prediction function for multiple IDs

"""
ensure that data.time and data.sigma is Float64
in data.sigma, NA are replaced by 0
"""

function jODE_sanitizeData(data)
    data = DataFrame(data)
    data.time = Float64.(data.time)
    if typeof(data.sigma[1]) == typeof("a")
        data.sigma[data.sigma .== "NA"] = ["0" for x in data.sigma[data.sigma .== "NA"]]
        data.sigma = parse.(Float64, data.sigma)
    end
    data
end

"""
Function factory to compute likelihood

...
# Arguments
- `est_mat,fixed_mat`: tables containing parameter lookup tables for pars to estimate and fixed pars. 
- `jODE_make_pars`: function returning two vectors of parameters: pars, fixed
- `jODE_(f|g|p|e)`: functions named as in dMod, for single condition. f is only the ode, is passed to the ode solver. 
  Choices are jODE_f, jODE_f2 and jODE_p, jODE_p2
- `data`: DataFrame with :name => String :time => Float64 :value => Float64 :sigma => Float64 :lloq => Float64 and :ID => Float64/Int64
- `jODE_prd_condition`: prediction function for single condition
- OLD: now sigma of data is always used except when data.sigma is NA. then, sigma_1 is taken `sigma`: symbol, choose between :sigma (take sigma from data) and :sigma_1 (take sigma from error model)
...
@return A function LL(est_vec) to compute a tuple (value, gradient, hessian) of the objective function
"""
function jODE_normL2(data, jODE_prd_condition, est_mat,
    fixed_mat, jODE_make_pars, jODE_p, jODE_f, jODE_g, jODE_e)
    
    IDs = est_mat[:,1]
    pars, fixed = jODE_make_pars(est_vec, est_mat, fixed_mat, 1)

    cn = ones(1)*IDs[1]
    
    function jODE_LL_condition(pars)
        mycn = cn[1]
        dummy, fixed = jODE_make_pars(est_vec, est_mat, fixed_mat, mycn)
        pred = jODE_prd_condition(pars, fixed, mycn, datatimes, jODE_p, jODE_f, jODE_g, jODE_e) 
        # prepare calculations
        # print("pred defined\n")
        df_both = join(data,pred, on = [:name, :time, :ID], kind = :inner, makeunique = true)
        df_both.is_bloq = df_both.value .< df_both.lloq
        df_both.objval = convert.(eltype(pars), zeros(length(df_both.lloq)))
        df_both.sigma  = convert.(eltype(pars), df_both.sigma)
        # choose correct sigma
        df_both.sigma[df_both.sigma .== 0.] = df_both.sigma_1[df_both.sigma .== 0.]
        # print("df_both defined\n")
        
        # split into aloq and bloq
        df_both = sort(df_both, :is_bloq)
        if any(df_both.is_bloq)
            df_aloq, df_bloq = groupby(df_both, :is_bloq)
        else 
            df_aloq = df_both
        end
        # print("df_aloq and df_bloq defined\n")
                
        # calculate LL
        # [] Include opt.M4:
        df_aloq.objval  =  @. log(2π) + 2*log(df_aloq[!, :sigma]) + ((df_aloq[!, :value_1]-df_aloq[!, :value])/df_aloq[!, :sigma])^2
        # print("df_aloq.objval defined\n")
        if @isdefined df_bloq
            df_bloq.objval = @. -2*StatsFuns.normlogcdf(df_bloq[!, :value_1], df_bloq[!, :sigma], df_bloq[!, :lloq])
        end
        # print("df_bloq.objval defined\n")
        
        # Objective value
        # summing df_both works because df_aloq and df_bloq are not deep copies
        sum(df_both.objval)
        
    end
    
    # Calculate with derivs
    function jODE_LL_deriv(est_vec; verbose = false, deriv = true)
        
        val, grd, hes = 0., zeros(length(est_vec)), zeros(length(est_vec),length(est_vec))
        pars, dummy2 = jODE_make_pars(est_vec, est_mat, fixed_mat, cn)
        result = DiffResults.HessianResult(pars)
        for ID in IDs 
            cn[1] = ID
            verbose&&println("ID $(cn[1])")
            pars, dummy2 = jODE_make_pars(est_vec, est_mat, fixed_mat, ID)
            # [] Play around with chunk size?
            # verbose&&println(pars)
            # verbose&&println(fixed)
            # ID == 1 && write("wup$(max(IDs...)).txt", "$pars", "$fixed")
            if deriv
                result = ForwardDiff.hessian!(result, jODE_LL_condition, pars);
                val += DiffResults.value(result)
                indices = est_mat[convert(Int64,ID), 2:end]
                grd[indices] += [DiffResults.gradient(result)...;]
                hes[indices, indices] += DiffResults.hessian(result)
            else
                val += jODE_LL_condition(pars)
            end
        end

        if deriv
            return (val, grd, hes)
        else
            return val
        end
    end
  
end

