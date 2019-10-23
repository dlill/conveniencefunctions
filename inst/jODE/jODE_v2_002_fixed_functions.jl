"""
Function to extract pars,fixed from est_vec, est_mat and fixed_mat for a secific condition

Use with output from R function jODE_01_prepare_mats

...
# Arguments
- `est_vec`: vector containing all outer parameters to be estimated
- `est_mat`: lookup table for pars to be estimated for specific condition. 
- `fixed_mat`: table containing fixed pars
- `condition`: condition identifier
...
"""
function jODE_make_pars(est_vec, est_mat, fixed_mat, condition)
    pars  = est_vec[est_mat[    est_mat[:,1] .== condition, 2:end]]
    fixed =      fixed_mat[fixed_mat[:,1] .== condition, 2:end]
    return(pars, fixed)
end # Make pars function: Create pars and fixed from pars, est_mat, fixed_mat, condition


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
Function factory to compute likelihood

...
# Arguments
- `est_mat,fixed_mat`: tables containing parameter lookup tables for pars to estimate and fixed pars. 
- `jODE_make_pars`: function returning two vectors of parameters: pars, fixed
- `jODE_(f|g|p|e)`: functions named as in dMod, for single condition. f is only the ode, is passed to the ode solver. 
  Choices are jODE_f, jODE_f2 and jODE_p, jODE_p2
- `data`: DataFrame with :name => String :time => Float64 :value => Float64 :sigma => Float64 :lloq => Float64 and :ID => Float64/Int64
- `jODE_prd_condition`: prediction function for single condition
- `sigma`: symbol, choose between :sigma (take sigma from data) and :sigma_1 (take sigma from error model)
...
@return A function LL(est_vec) to compute a tuple (value, gradient, hessian) of the objective function
"""
function jODE_normL2(data, jODE_prd_condition, est_mat,
    fixed_mat, jODE_make_pars, jODE_p, jODE_f, jODE_g, jODE_e,
    sigma
    )
    
    datatimes = convert.(Float64, unique(data.time))
    IDs = unique(data.ID)
    pars, fixed = jODE_make_pars(est_vec, est_mat, fixed_mat, 1)
    
    cn = ones(1)*IDs[1]
    
    function jODE_LL_condition(pars)
        mycn = cn[1]
        dummy, fixed = jODE_make_pars(est_vec, est_mat, fixed_mat, mycn)
        pred = jODE_prd_condition(pars, fixed, mycn, datatimes, jODE_p, jODE_f, jODE_g, jODE_e) 
        # prepare calculations
        # print("pred defined\n")
        df_both = join(data,pred, on = [:name, :time, :condition], kind = :inner, makeunique = true)
        df_both = @transform(df_both, 
                                is_bloq = :value .< :lloq, 
                                objval = convert.(eltype(pars), zeros(length(:lloq))))
        df_both = sort(df_both, :is_bloq)
        # print("df_both defined\n")
        
        # split into aloq and bloq
        if any(df_both.is_bloq)
            df_aloq, df_bloq = groupby(df_both, :is_bloq)
        else 
            df_aloq = df_both
        end
        # print("df_aloq and df_bloq defined\n")
        
        
        # calculate LL
        # [] Include opt.M4:
        df_aloq.objval  =  @. log(2π) + 2*log(df_aloq[sigma]) + ((df_aloq[:value_1]-df_aloq[:value])/df_aloq[sigma])^2
        # print("df_aloq.objval defined\n")
        if @isdefined df_bloq
            df_bloq.objval = @. -2*StatsFuns.normlogcdf(df_bloq[:value_1], df_bloq[sigma], df_bloq[:lloq])
        end
        # print("df_bloq.objval defined\n")
        
        # Objective value
        # summing df_both works because df_aloq and df_bloq are not deep copies
        sum([df_both.objval...])
        
    end
    
    # Calculate with derivs
    function jODE_LL_deriv(est_vec; verbose = false, deriv = true)
        
        val, grd, hes = 0., zeros(length(est_vec)), zeros(length(est_vec),length(est_vec))
        pars, dummy2 = jODE_make_pars(est_vec, est_mat, fixed_mat, cn)
        result = DiffResults.HessianResult(pars)
        for condition in IDs 
            cn[1] = condition
            # verbose&&println("condition $(cn[1])")
            pars, dummy2 = jODE_make_pars(est_vec, est_mat, fixed_mat, condition)
            # [] Play around with chunk size?
            # verbose&&println(pars)
            # verbose&&println(fixed)
            # condition == 1 && write("wup$(max(IDs...)).txt", "$pars", "$fixed")
            if deriv
                result = ForwardDiff.hessian!(result, jODE_LL_condition, pars);
                val += DiffResults.value(result)
                indices = est_mat[convert(Int64,condition), 2:end]
                grd[indices] += [DiffResults.gradient(result)...;]
                hes[indices, indices] += DiffResults.hessian(result)
            else
                val += jODE_LL_condition(pars)
            end
        end

        return (val, grd, hes)
    end
  
end

