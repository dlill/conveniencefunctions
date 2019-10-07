using PackageCompiler

cs = PackageCompiler.current_systemimage()

#=
# uncomment to restore backup
cp(replace(cs, r"\.so" => "_bak.so"), cs,force = true)
=#

#=
# uncomment to create backup (also set force to true)
try
    cp(cs, replace(cs, r"\.so" => "_bak.so"), force = false)
catch e
    print(e)
end
=#

#=
# Uncomment to create new sys.image
PackageCompiler.compile_incremental(
    :JLD, 
    :CSV,
    force = true)
=#