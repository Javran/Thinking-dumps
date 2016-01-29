function my_pfactorial(n)
    # this implementation is slow
    # because the amount of work on each process is too
    # small that the extra cost of putting this into parallelism
    # negates the benefit.
    @parallel (*) for i = 1:n
        i
    end
end

function my_factorial(n)
    p = 1
    for i = 1:n
        p = p*i
    end
    p
end

# correctness
println( map(my_pfactorial, collect(1:10)) )
println( map(my_factorial, collect(1:10)) )
println( map(factorial, collect(1:10)) )

# @time my_pfactorial(10000)
# @time my_factorial(10000)
chunk_size = 1000
cell_count = Int64(ceil(12345 / chunk_size))
plan = Array{Array{Int64}}( cell_count )

j = 0
for i = 1:chunk_size:12345
    j = j+1
    plan[j] = [i,min(i+chunk_size-1,12345)]
end

println(plan)
