println( typeof(Symbol) )
println( typeof(Int64) )
# both are of type "DataType"

println( typeof( typeof ) )
println( typeof( in ) )
# "Function"

typedDict = Dict{Symbol,Float64}(:a => 1, :b => 2)
typedDict[:c] = 3

# typedDict[:thisis] = :notanumber
# causes an error because there's no (generally accepted) way
# to convert symbols in floating numbers

println(typedDict)
