function my_pfactorial(n)
    # this implementation is slow
    # because the amount of work on each process is too
    # small that the extra cost of putting this into parallelism
    # negates the benefit.
    @parallel (*) for i = 1:n
        BigInt(i)
    end
end

function my_factorial(n)
    p = BigInt(1)
    for i = 1:n
        p = p*BigInt(i)
    end
    p
end

# only faster for big numbers
function my_fast_factorial(n :: Int)
    chunk_size = 1000
    cell_count = Int64(ceil(n/ chunk_size))
    plan = Array{Array{Int64}}( cell_count )

    j = 0
    for i = 1:chunk_size:n
        j = j+1
        plan[j] = [i,min(i+chunk_size-1,n)]
    end

    @parallel (*) for (from,to) = plan
        p = BigInt(1)
        for i = from:to
            p = p*BigInt(i)
        end
        p
    end
end

# correctness
function simple_test( func )
    println( map(func, collect(1:10)) )
end

function measure_time( func )
    # just a random (prime) number
    # for testing speed
    @time func(12269)
end

functions = [my_pfactorial, my_factorial, my_fast_factorial]
map(simple_test, functions)

r1,r2,r3 = map(measure_time, functions)

assert( r1 == r2 && r2 == r3 )

# seems "concat" is not used any more,
# but let's just do what we are asked.
function concat(n :: Int64, mat :: Array{Int64})
    hcat( fill(n, size(mat)),mat)
end

println(concat(5,[1 2;3 4]))
