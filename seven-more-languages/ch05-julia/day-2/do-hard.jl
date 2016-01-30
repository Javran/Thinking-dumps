function test_flip(func)
    result = @time pflip_coins(100000)
    println( result )
end

# pflip_coins in book,
# modified to count total number of experiments done
function pflip_coins(times)
    @parallel (+) for i = 1:times
        [Int(rand(Bool)),1]
    end
end

# test_flip( pflip_coins )

# we can optimize "pflip_coins" by partitioning tasks into "bigger" pieces:
# instead of fliping one single coins, each task can flip coin multiple times.
function pflip_coins_fast(times)
    chunk_size = 1000
    tasklist_size = Int64( ceil(times/chunk_size) )

    tasks = fill(chunk_size, tasklist_size)
    last = mod(times, chunk_size)
    if last != 0
        tasks[end] = last
    end

    println(tasks)
end

pflip_coins_fast(2000)
