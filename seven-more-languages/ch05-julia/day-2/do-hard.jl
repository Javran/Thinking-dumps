function test_flip(func)
    result = @time pflip_coins(1000000)
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

# this is the task to be performed by each worker.
function flip_coin_experiment(times)
    total = 0
    head = 0
    for i = 1:times
        total = total + 1
        if rand(Bool)
            head = head + 1
        end
    end
    [head, total]
end

function pflip_coins_fast(times)
    chunk_size = 1000
    tasklist_size = Int64( ceil(times/chunk_size) )

    tasks = fill(chunk_size, tasklist_size)
    last = mod(times, chunk_size)
    if last != 0
        tasks[end] = last
    end

    results = map( (x -> @fetch flip_coin_experiment(x)), tasks)
    reduce(+, [0,0], results)
end

test_flip(pflip_coins)
test_flip(pflip_coins_fast)

# the optimization is significant:

# with "-p 8":
#   1.053630 seconds (467.31 k allocations: 20.020 MB, 0.32% gc time)
# [500551,1000000]
#   0.087363 seconds (8.71 k allocations: 670.547 KB)
# [499360,1000000]

# without any extra arguments:
#   0.316822 seconds (3.20 M allocations: 176.912 MB, 8.76% gc time)
# [500329,1000000]
#   0.102915 seconds (3.00 M allocations: 167.848 MB, 6.96% gc time)
# [499460,1000000]
