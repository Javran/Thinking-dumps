# the syntax part of the language is a bit hard to find
# in the manual, try searching for "Range".

for i = 10:-1:1
    println(i)
end

for i = [1 2 3; 4 5 6; 7 8 9]
    println("<< $i >>")
end

# if i = 1:10 iterates by 1,2,3
# then this should intuitively be [1;4;7], [2;5;8] or so
# however the result here looks rather bizarre:
# 1,4,7,2,5,8,3,6,9
# could be the case that matrix are iterated element-wise,
# in column major order...


# not sure what "trial count" means, let's just try counting flips
@everywhere function pflip_coins(times)
    @parallel (+) for i = 1:times
        Int(rand(Bool))
    end
end

r = pmap(pflip_coins, [100,200,300,400,500])
println("Waiting ...")

result = fetch(r)
println(result)
