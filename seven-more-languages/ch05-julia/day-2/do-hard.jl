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

println( pflip_coins_fast(10023) )
