# Todolist

[] PETab support
[] outputting functions which create the directory if not existent
[] theme_cf, scale_colorfill_cf(aesthetics = c("color", "fill"))
[] rework cf_head snippet
[] more debugging power for dMod models. Whenever anything helped you debugging a model, just dump it here
[] Good functionality to handle fit/profile results
[] Folder structure for models like sysProject: Copy the data which went into the fit, consistent fitting
[] * idea: cf_mstrust(mstrustargs..., outputFolder) writes standard files to the outputFolder
[]    * "summary.txt", "waterfall.png", "waterfall_10best.png", "mstrustParframe.csv", "pars.png", "pars_10best.png"
[] * idea: cf_profile(profileArgs..., outputFolder) same for profiles
[]    * Could be sth like "profiled_pars.txt" = markdown table with columns c(name, min, optimum, max), "profileParframe.csv" the result, "modelInfo.txt" some info about conditions, priors, data points etc...
[] Nice wrappers for runbg: Too many lines of code, but dont know myself how to improve this ...

# General remarks

* No guarantee for working code, but one could think of releasing stable versions
* Feel free to change things but maybe notify the others with a short issue on github (no pull requests, this is overkill :) )
