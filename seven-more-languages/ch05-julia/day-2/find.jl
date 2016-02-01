@everywhere include("Common.jl")

# @everywhere include is just not working, don't try it.
@everywhere function pflip_coins(times)
    @parallel (+) for i = 1:times
        Int(rand(Bool))
    end
end

# not sure why but seems starting Julia takes quite a lot time
println("Started.")

# NOTE: in Julia 0.4.4, the order of the arguments are changed
# and the first argument becomes the id
r = @spawn pflip_coins(10000)

# the previous call should return immediately regardless
# of how long the actual computation would take
# which allow us to do stuff while the previous computation is running
println("Waiting ...")

# now we block until the result is available
result = fetch(r)
println("Done.")

println( result )
